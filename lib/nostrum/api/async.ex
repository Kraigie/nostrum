defmodule Nostrum.Api.Async do
  @moduledoc """
  This is the asyncified version of `Nostrum.Api`.

  Most of the functions are the same, except they return a `Nostrum.Api.Promise.t()`
  Which can be piped into `await/1` to wait for the response.

  NOTE: Due to limitations of `:gen_statem`, which is used to make requests,
  a promise returned by any function in this module must be awaited on by the same process that created it.
  """

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  alias Nostrum.Api.Ratelimiter
  alias Nostrum.{Constants, Util}

  alias Nostrum.Struct.{Channel, Message}

  alias Nostrum.Api.Promise

  @type options :: keyword | map

  @type error :: {:error, Nostrum.Error.ApiError.t()}

  defguardp has_files(args) when is_map_key(args, :files) or is_map_key(args, :file)

  @spec await(Promise.t(), :infinity | non_neg_integer | {:abs, integer}) :: any()
  def await(%Promise{request_id: request_id, callback: callback}, timeout \\ :infinity) do
    {:reply, response} = :gen_statem.wait_response(request_id, timeout)
    callback.(response)
  end

  @doc """
  Awaits on a list of promises, returning a list with the responses.

  If a timout is given, it will return a `:timeout` tuple with any responses that were received
  should the timeout be reached before all promises are fulfilled.
  """
  @spec await_many([Promise.t()], :infinity | non_neg_integer | {:abs, integer}) ::
          {:ok, [any()]} | {:timeout, [any()]}
  def await_many(promises, timeout \\ :infinity) do
    {req_id_collection, promise_map} =
      Enum.reduce(promises, {:gen_statem.reqids_new(), %{}}, fn promise, {req_ids, map} ->
        req_id = promise.request_id
        handle = promise.handle
        callback = promise.callback
        {:gen_statem.reqids_add(req_id, handle, req_ids), Map.put(map, handle, callback)}
      end)

    _await_many(req_id_collection, promise_map, timeout, [])
  end

  defp _await_many(req_id_collection, promise_map, timeout, results) do
    case :gen_statem.wait_response(req_id_collection, timeout, true) do
      {{:reply, resp}, handle, new_collection} ->
        callback = Map.get(promise_map, handle)
        _await_many(new_collection, promise_map, timeout, [callback.(resp) | results])

      :no_request ->
        {:ok, results}

      :timeout ->
        {:timeout, results}
    end
  end

  @spec create_message(Channel.id() | Message.t(), options | String.t()) ::
          Promise.t(error | {:ok, Message.t()})
  def create_message(channel_id, options)

  def create_message(%Message{} = message, options),
    do: create_message(message.channel_id, options)

  def create_message(channel_id, content) when is_binary(content),
    do: create_message(channel_id, %{content: content})

  def create_message(channel_id, options) when is_list(options),
    do: create_message(channel_id, Map.new(options))

  def create_message(channel_id, %{} = options) when is_snowflake(channel_id) do
    options = prepare_allowed_mentions(options) |> combine_embeds()

    callback = &handle_request_with_decode(&1, {:struct, Message})

    request(:post, Constants.channel_messages(channel_id), options, [], callback)
  end

  @spec request(atom(), String.t(), any, keyword() | map(), (term() -> term())) ::
          Promise.t({:ok} | {:ok, String.t()} | error)
  def request(method, route, body \\ "", params \\ [], callback \\ &Function.identity/1)

  def request(method, route, %{} = body, params, callback) when has_files(body),
    do: request_multipart(method, route, body, params, callback)

  def request(method, route, %{data: data} = body, params, callback) when has_files(data),
    do: request_multipart(method, route, body, params, callback)

  def request(method, route, body, params, callback) do
    %{
      method: method,
      route: route,
      body: body,
      params: params,
      headers: [{"content-type", "application/json"}]
    }
    |> make_request(callback)
  end

  def make_request(request_map, callback \\ &Function.identity/1) do
    request_id = :gen_statem.send_request(Ratelimiter, {:queue, request_map})
    handle = make_ref()

    %Promise{
      callback: callback,
      handle: handle,
      request_id: request_id
    }
  end

  @spec request_multipart(atom(), String.t(), any, keyword() | map(), (term() -> term())) ::
          Promise.t({:ok} | {:ok, String.t()} | error)
  def request_multipart(method, route, body, params \\ [], callback \\ &Function.identity/1) do
    boundary = generate_boundary()
    {files, body} = combine_files(body) |> pop_files()
    json = Jason.encode_to_iodata!(body)

    %{
      method: method,
      route: route,
      # Hello :gun test suite :^)
      body: {:multipart, create_multipart(files, json, boundary)},
      params: params,
      headers: [
        {"content-type", "multipart/form-data; boundary=#{boundary}"}
      ]
    }
    |> make_request(callback)
  end

  # If `:embed` is present, prepend to `:embeds` for compatibility
  defp combine_embeds(%{embed: embed} = args),
    do: Map.delete(args, :embed) |> Map.put(:embeds, [embed | args[:embeds] || []])

  defp combine_embeds(%{data: data} = args), do: %{args | data: combine_embeds(data)}
  defp combine_embeds(%{message: data} = args), do: %{args | message: combine_embeds(data)}
  defp combine_embeds(args), do: args

  # If `:file` is present, prepend to `:files` for compatibility
  defp combine_files(%{file: file} = args),
    do: Map.delete(args, :file) |> Map.put(:files, [file | args[:files] || []])

  defp combine_files(%{data: data} = args), do: %{args | data: combine_files(data)}
  defp combine_files(%{message: data} = args), do: %{args | message: combine_files(data)}
  defp combine_files(args), do: args

  defp pop_files(%{data: data} = args),
    do: {data.files, %{args | data: Map.delete(data, :files)}}

  defp pop_files(%{message: data} = args),
    do: {data.files, %{args | message: Map.delete(data, :files)}}

  defp pop_files(args), do: Map.pop!(args, :files)

  @doc false
  def bangify(to_bang) do
    case to_bang do
      {:error, error} ->
        raise(error)

      {:ok, body} ->
        body

      {:ok} ->
        {:ok}
    end
  end

  @doc """
  Returns the token of the bot.
  """
  @spec get_token() :: String.t()
  def get_token do
    Application.get_env(:nostrum, :token)
  end

  defp handle_request_with_decode(response)
  defp handle_request_with_decode({:ok, body}), do: {:ok, Jason.decode!(body, keys: :atoms)}
  defp handle_request_with_decode({:error, _} = error), do: error

  defp handle_request_with_decode(response, type)
  # add_guild_member/3 can return both a 201 and a 204
  defp handle_request_with_decode({:ok}, _type), do: {:ok}
  defp handle_request_with_decode({:error, _} = error, _type), do: error

  defp handle_request_with_decode({:ok, body}, type) do
    convert =
      body
      |> Jason.decode!(keys: :atoms)
      |> Util.cast(type)

    {:ok, convert}
  end

  defp prepare_allowed_mentions(options) do
    with raw_options when raw_options != :all <- Map.get(options, :allowed_mentions, :all),
         allowed_mentions when is_map(allowed_mentions) <- parse_allowed_mentions(raw_options) do
      Map.put(options, :allowed_mentions, allowed_mentions)
    else
      _ ->
        Map.delete(options, :allowed_mentions)
    end
  end

  @crlf "\r\n"

  defp create_multipart(files, json, boundary) do
    json_mime = MIME.type("json")
    json_size = :erlang.iolist_size(json)

    file_parts =
      files
      |> Enum.with_index(0)
      |> Enum.map(fn {f, i} -> create_file_part_for_multipart(f, i, boundary) end)

    [
      ~s|--#{boundary}#{@crlf}|,
      file_parts
      | [
          ~s|content-length: #{json_size}#{@crlf}|,
          ~s|content-type: #{json_mime}#{@crlf}|,
          ~s|content-disposition: form-data; name="payload_json"#{@crlf}#{@crlf}|,
          json,
          ~s|#{@crlf}--#{boundary}--#{@crlf}|
        ]
    ]
  end

  defp create_file_part_for_multipart(file, index, boundary) do
    {body, name} = get_file_contents(file)

    file_mime = MIME.from_path(name)
    file_size = :erlang.iolist_size(body)

    [
      ~s|content-length: #{file_size}#{@crlf}|,
      ~s|content-type: #{file_mime}#{@crlf}|,
      ~s|content-disposition: form-data; name="files[#{index}]"; filename="#{name}"#{@crlf}#{@crlf}|,
      body,
      ~s|#{@crlf}--#{boundary}#{@crlf}|
    ]
  end

  defp get_file_contents(path) when is_binary(path) do
    {File.read!(path), Path.basename(path)}
  end

  defp get_file_contents(%{body: body, name: name}), do: {body, name}

  defp generate_boundary do
    String.duplicate("-", 20) <>
      "KraigieNostrumCat_" <>
      Base.encode16(:crypto.strong_rand_bytes(10))
  end

  defp parse_allowed_mentions(:none), do: %{parse: []}
  defp parse_allowed_mentions(:everyone), do: %{parse: [:everyone]}

  # Parse users
  defp parse_allowed_mentions(:users), do: %{parse: [:users]}
  defp parse_allowed_mentions({:users, users}) when is_list(users), do: %{users: users}

  # Parse roles
  defp parse_allowed_mentions(:roles), do: %{parse: [:roles]}
  defp parse_allowed_mentions({:roles, roles}) when is_list(roles), do: %{roles: roles}

  # Parse many
  defp parse_allowed_mentions(options) when is_list(options) or is_map(options) do
    options
    |> Enum.map(&parse_allowed_mentions/1)
    |> Enum.reduce(fn a, b ->
      Map.merge(a, b, fn
        key, parse_a, parse_b when key in [:parse, :users, :roles] ->
          Enum.uniq(parse_a ++ parse_b)

        _k, _v1, v2 ->
          v2
      end)
    end)
    |> Map.put_new(:parse, [])
  end

  # ignore
  defp parse_allowed_mentions(options), do: options
end
