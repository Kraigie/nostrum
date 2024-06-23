defmodule Nostrum.Shard.Session do
  @moduledoc """
  Manages a single shard's gateway connection.


  ## Purpose

  Discord's gateway sends us events over websocket. The shard session state
  machine concerns it self with parsing these events and dispatching them to
  clients as appropriate.


  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.


  ## Inner workings

  The session is implemented via `:gen_statem` and can be in one of the
  following states:

  - `disconnected`: when no connection is up at all. On initial connection of
  the session (e.g. no `seq` field is available), this will block if we need to
  wait a moment to respect the session startup concurrency limits. Afterwards,
  it will transition to `connecting_http`.

  - `connecting_http`: We are setting up a HTTP connection to the API. This
  means that no connection was available previously at all, and we need to open
  it from scratch. Used for gateway-initiated reconnect requests ("Cloudflare
  Websocket proxy restarting") and on `:gun_down` notifications for the
  connection in connected state. Once `:gun` notifies us that the connection is
  up, we transition to the `connecting_ws` state.

  - `connecting_ws`: We are turning the HTTP connection into a WebSocket
  connection. If this succeeds, we head into the `connected` state.

  - `connected`: The WebSocket connection is up. This state actively deals with
  new data from the gateway, and takes care of heartbeating. If Discord fails
  to respond to our heartbeats, we close down the full connection and attempt
  to re-establish and resume events.
  """

  alias Nostrum.{Constants, ConsumerGroup, Util}
  alias Nostrum.Shard.{Connector, Event, Payload}
  alias Nostrum.Struct.WSState

  alias Nostrum.Shard.Session.Compression

  require Logger

  @behaviour :gen_statem

  # Query string to connect to when upgrading the connection.
  @gateway_qs "/?encoding=etf&v=10"

  @gateway_compress Application.compile_env(
                      :nostrum,
                      :gateway_compression,
                      :zlib
                    )

  @compression_module (case @gateway_compress do
                         :zlib ->
                           Compression.Zlib

                         :zstd ->
                           Compression.Zstd.check_available!()
                           Compression.Zstd

                         _ ->
                           raise ArgumentError,
                                 "Unsupported compression type: #{@gateway_compress}"
                       end)

  # Maximum time the initial connection may take.
  @timeout_connect :timer.seconds(5)
  # Maximum time the websocket upgrade may take.
  @timeout_ws_upgrade :timer.seconds(5)
  # Messages to buffer at a time. Decremented by :gun by 1 for every message we
  # receive. If this reaches zero, `:gun` will stop reading events from
  # upstream. Equivalent to setting `{active, false}` on the socket at `0`.
  @standard_flow 10

  def update_status(pid, status, game, stream, type) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}

        _ ->
          {0, false}
      end

    payload = Payload.status_update_payload(idle_since, game, stream, status, afk, type)
    :gen_statem.cast(pid, {:status_update, payload})
  end

  def update_voice_state(pid, guild_id, channel_id, self_mute, self_deaf) do
    payload = Payload.update_voice_state_payload(guild_id, channel_id, self_mute, self_deaf)
    :gen_statem.cast(pid, {:update_voice_state, payload})
  end

  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    :gen_statem.cast(pid, {:request_guild_members, payload})
  end

  def disconnect(pid) do
    :gen_statem.call(pid, {:disconnect, nil})
  end

  def disconnect(pid, timeout) do
    :gen_statem.call(pid, {:disconnect, nil}, timeout)
  end

  def get_ws_state(pid) do
    :sys.get_state(pid)
  end

  # State machine API

  def start_link({:connect, [_gateway, _shard_num, _total]} = opts, statem_opts) do
    :gen_statem.start_link(__MODULE__, opts, statem_opts)
  end

  def start_link(
        {:reconnect,
         %{
           shard_num: _shard_num,
           total_shards: _total_shards,
           gateway: _gateway,
           resume_gateway: _resume_gateway,
           seq: _seq,
           session: _session
         }} =
          opts,
        statem_opts
      ) do
    :gen_statem.start_link(__MODULE__, opts, statem_opts)
  end

  def start_link([_gateway, _shard_num, _total] = shard_opts, statem_opts) do
    :gen_statem.start_link(__MODULE__, shard_opts, statem_opts)
  end

  def init({:connect, [gateway, shard_num, total]}) do
    Logger.metadata(shard: shard_num)

    state = %WSState{
      conn_pid: self(),
      shard_num: shard_num,
      total_shards: total,
      gateway: gateway
    }

    connect = {:next_event, :internal, :connect}
    {:ok, :disconnected, state, connect}
  end

  def init(
        {:reconnect,
         %{
           shard_num: shard_num,
           total_shards: total_shards,
           gateway: gateway,
           resume_gateway: resume_gateway,
           seq: seq,
           session: session
         }}
      ) do
    Logger.metadata(shard: shard_num)

    state = %WSState{
      conn_pid: self(),
      shard_num: shard_num,
      total_shards: total_shards,
      gateway: gateway,
      resume_gateway: resume_gateway,
      session: session,
      seq: seq
    }

    connect = {:next_event, :internal, :connect}
    {:ok, :disconnected, state, connect}
  end

  def init([gateway, shard_num, total]) do
    init({:connect, [gateway, shard_num, total]})
  end

  def callback_mode, do: [:state_functions, :state_enter]

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts, []]},
      type: :worker,
      restart: :transient,
      significant: true,
      shutdown: 500
    }
  end

  # Ok, we just came here, and we're disconnected. Before we try to connect,
  # we need to wait for the shard connector to tell us it's okay to connect.
  def disconnected(:enter, _previous_state, %{seq: nil} = data) do
    # XXX: Should this be non-blocking?
    Logger.debug("Establishing initial connection")
    :ok = Connector.block_until_connect()
    {:next_state, :disconnected, data}
  end

  # We've had a connection before. Since we won't identify with IDENTIFY
  # but with RESUME, we don't need to respect the concurrency limit, as
  # it only applies to identify.
  def disconnected(:enter, _previous_state, data) do
    Logger.debug("Reconnecting with seq token")
    {:next_state, :disconnected, data}
  end

  def disconnected(:internal, :connect, data) do
    {:next_state, :connecting_http, data}
  end

  # def disconnected(_kind, _request, _data) do
  #  {:keep_state_and_data, :postpone}
  # end

  # If we've been here before, we want to use the resume gateway URL to connect
  # instead of the regular gateway URL.
  #
  # We also have to strip the "wss://" prefix from the URL, as `:gun` doesn't
  # currently understand it.
  def connecting_http(:enter, from, %{resume_gateway: "wss://" <> resume_gateway} = data) do
    Logger.debug("Resuming on #{inspect(resume_gateway)}")

    # if we don't set resume_gateway to nil, we'll have an infinite loop
    connecting_http(:enter, from, %{data | gateway: resume_gateway, resume_gateway: nil})
  end

  def connecting_http(:enter, _from, %{gateway: gateway} = data) do
    set_timeout = {:state_timeout, @timeout_connect, :connect_timeout}

    gun_opts = %{
      connect_timeout: :timer.seconds(5),
      domain_lookup_timeout: :timer.seconds(5),
      # Do not retry. The state machine wants to keep track of this on its own terms.
      retry: 0,
      protocols: [:http],
      tls_handshake_timeout: :timer.seconds(5),
      tls_opts: Constants.gun_tls_opts()
    }

    {:ok, worker} = :gun.open(:binary.bin_to_list(gateway), 443, gun_opts)
    # _monitor = :erlang.monitor(:process, worker)
    {:keep_state, %{data | conn: worker}, set_timeout}
  end

  def connecting_http(:info, {:gun_up, conn, :http}, %{conn: conn} = data) do
    {:next_state, :connecting_ws, data}
  end

  def connecting_http(:state_timeout, :connect_timeout, _data) do
    {:stop, :connect_http_timeout}
  end

  # def connecting_http(_kind, _request, _data) do
  #  {:keep_state_and_data, :postpone}
  # end

  def connecting_ws(:enter, _from, %{conn: conn} = data) do
    Logger.debug("Upgrading connection to websocket with #{@gateway_compress} compression")
    set_timeout = {:state_timeout, @timeout_ws_upgrade, :upgrade_timeout}
    stream = :gun.ws_upgrade(conn, @gateway_qs <> compression_qs(), [], %{flow: @standard_flow})
    {:keep_state, %{data | stream: stream}, set_timeout}
  end

  def connecting_ws(
        :info,
        {:gun_upgrade, _conn, _stream, ["websocket"], _headers},
        %{compress_ctx: nil} = data
      ) do
    context = @compression_module.create_context()

    {:next_state, :connected,
     %{
       data
       | compress_ctx: context,
         last_heartbeat_ack: DateTime.utc_now(),
         heartbeat_ack: true
     }}
  end

  def connecting_ws(
        :info,
        {:gun_upgrade, _conn, _stream, ["websocket"], _headers},
        %{compress_ctx: compress_ctx} = data
      ) do
    Logger.info("Re-established websocket connection")

    compress_ctx = @compression_module.reset_context(compress_ctx)

    {:next_state, :connected,
     %{
       data
       | last_heartbeat_ack: DateTime.utc_now(),
         heartbeat_ack: true,
         compress_ctx: compress_ctx
     }}
  end

  def connecting_ws(:state_timeout, :upgrade_timeout, _data) do
    {:stop, :connect_ws_timeout}
  end

  # def connecting_ws(_kind, _request, _data) do
  #   {:keep_state_and_data, :postpone}
  # end

  # We don't need to specially handle resuming here, because Shard.Event will
  # adjust our initial payload accordingly.
  def connected(:enter, _from, _data) do
    {reference, consumers} = ConsumerGroup.monitor()

    case consumers do
      [] ->
        Logger.debug("Shard connection up, waiting for consumers to boot")
        :ok = wait_for_consumer_boot(reference, :timer.seconds(5))
        Logger.debug("Consumer up, we are ready to rumble")

      _consumers ->
        Logger.info("Shard connection armed and ready")
    end

    :ok = ConsumerGroup.demonitor(reference)
    :keep_state_and_data
  end

  def connected(:info, {:gun_ws, _worker, stream, {:binary, frame}}, data) do
    payload_decompressed = @compression_module.inflate(data.compress_ctx, frame)

    payload =
      payload_decompressed
      |> :erlang.iolist_to_binary()
      |> :erlang.binary_to_term()

    data_with_seq = %{data | seq: payload.s || data.seq}

    {from_handle, heartbeat_actions} =
      payload.op
      |> Constants.atom_from_opcode()
      |> Event.handle(payload, data_with_seq)

    :ok = :gun.update_flow(data_with_seq.conn, stream, @standard_flow)

    case from_handle do
      {updated_data, :reconnect} ->
        Logger.info("Will reconnect in response to gateway event")
        {:keep_state, updated_data, {:next_event, :internal, :reconnect}}

      {updated_data, reply} ->
        :ok = :gun.ws_send(data_with_seq.conn, stream, {:binary, reply})
        {:keep_state, updated_data, heartbeat_actions}

      updated_data ->
        {:keep_state, updated_data, heartbeat_actions}
    end
  end

  def connected(:info, {:gun_ws, conn, stream, :close}, %{conn: conn, stream: stream}) do
    Logger.info("Shard websocket closed (unknown reason)")
    {:keep_state_and_data, {:next_event, :internal, :reconnect}}
  end

  def connected(:info, {:gun_ws, conn, _stream, {:close, errno, reason}}, %{conn: conn}) do
    Logger.info("Shard websocket closed (errno #{errno}, reason #{inspect(reason)})")
    {:keep_state_and_data, {:next_event, :internal, :reconnect}}
  end

  def connected(
        :info,
        {:gun_down, conn, _proto, _reason, _killed_streams},
        %{conn: conn}
      ) do
    Logger.info("Lost complete shard connection. Attempting reconnect.")
    {:keep_state_and_data, {:next_event, :internal, :reconnect}}
  end

  def connected(
        :info,
        {:gun_down, old_conn, _proto, _reason, _killed_streams},
        %{conn: new_conn}
        # technically the guard is not needed because of the above clause,
        # but it makes the intent a bit clearer
      )
      when old_conn != new_conn do
    Logger.debug("Received gun_down message for a previous shard connection. Ignoring message.")
    :keep_state_and_data
  end

  def connected(:cast, {request, payload}, %{conn: conn, stream: stream})
      when request in [:status_update, :update_voice_state, :request_guild_members] do
    :ok = :gun.ws_send(conn, stream, {:binary, payload})
    :keep_state_and_data
  end

  def connected({:call, from}, {:disconnect, nil}, %{
        conn: conn,
        shard_num: shard_num,
        total_shards: total,
        gateway: gateway,
        resume_gateway: resume_gateway,
        seq: seq,
        session: session
      }) do
    :ok = :gun.close(conn)
    :ok = :gun.flush(conn)

    {:stop_and_reply, :normal,
     {:reply, from,
      %{
        shard_num: shard_num,
        total_shards: total,
        gateway: gateway,
        resume_gateway: resume_gateway,
        session: session,
        seq: seq
      }}}
  end

  def connected(
        :state_timeout,
        :send_heartbeat = request,
        %{
          conn: conn,
          seq: seq,
          stream: stream,
          heartbeat_ack: heartbeat_ack,
          heartbeat_interval: heartbeat_interval
        } = data
      ) do
    if heartbeat_ack do
      # Our last heartbeat was acknowledged. Send another one.
      :ok = :gun.ws_send(conn, stream, {:binary, Payload.heartbeat_payload(seq)})
      heartbeat_later = {:state_timeout, heartbeat_interval, request}

      {:keep_state, %{data | heartbeat_ack: false, last_heartbeat_send: DateTime.utc_now()},
       heartbeat_later}
    else
      # Our last heartbeat was not acknowledged. Disconnect and try to resume.
      Logger.warning("Heartbeat ack not received in time, reconnecting")
      :ok = :gun.ws_send(conn, stream, :close)
      connect = {:next_event, :internal, :connect}
      {:next_state, :disconnected, %{data | stream: nil}, connect}
    end
  end

  # Internal event to force a complete reconnection from the connected state.
  # Useful when the gateway told us to do so.
  def connected(:internal, :reconnect, %{conn: conn} = data) do
    :ok = :gun.close(conn)
    :ok = :gun.flush(conn)
    connect = {:next_event, :internal, :connect}
    {:next_state, :disconnected, %{data | conn: nil, stream: nil}, connect}
  end

  # :gen_statem callback helper that removes the huge zlib data blob from any
  # errors experienced by the shard session state machine. This zlib blob is
  # only decodable by the zlib context that the error ocurred in anyway so it's
  # inclusion in bug reports only hinders. It has also already been decoded by
  # the time it reaches this call so we cannot attempt to decode it here.
  def format_status(%{queue: queue} = state) do
    queue =
      Enum.map(queue, fn queue_item ->
        case queue_item do
          # match gun websocket messages and remove the data blob
          {:info, {:gun_ws, conn, stream, {:binary, _payload}}} ->
            {:info, {:gun_ws, conn, stream, {:binary, "PAYLOAD REMOVED"}}}

          _ ->
            queue_item
        end
      end)

    %{state | queue: queue}
  end

  # Internal helper. Wait for consumers to start up.
  defp wait_for_consumer_boot(reference, timeout) do
    receive do
      {^reference, :join, _group, _who} ->
        :ok
    after
      timeout ->
        Logger.error(
          "No consumers were running nor did any start up in time " <>
            "for shard session startup. Is a consumer started as " <>
            "part of your supervision tree?"
        )

        :timeout
    end
  end

  defp compression_qs do
    "&compress=" <> Atom.to_string(@gateway_compress) <> "-stream"
  end
end
