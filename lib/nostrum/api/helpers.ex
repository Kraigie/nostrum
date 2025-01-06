defmodule Nostrum.Api.Helpers do
  @moduledoc false

  alias Nostrum.Util

  defguard has_files(args) when is_map_key(args, :files) or is_map_key(args, :file)

  def handle_request_with_decode(response)
  def handle_request_with_decode({:ok, body}), do: {:ok, Jason.decode!(body, keys: :atoms)}
  def handle_request_with_decode({:error, _} = error), do: error

  def handle_request_with_decode(response, type)
  # add_guild_member/3 can return both a 201 and a 204
  def handle_request_with_decode({:ok}, _type), do: {:ok}
  def handle_request_with_decode({:error, _} = error, _type), do: error

  def handle_request_with_decode({:ok, body}, type) do
    convert =
      body
      |> Jason.decode!(keys: :atoms)
      |> Util.cast(type)

    {:ok, convert}
  end

  @spec maybe_add_reason(String.t() | nil, list()) :: list()
  def maybe_add_reason(reason, headers \\ [{"content-type", "application/json"}])
  def maybe_add_reason(nil, headers), do: headers

  def maybe_add_reason(reason, headers) do
    [{"x-audit-log-reason", reason} | headers]
  end

  @spec maybe_convert_date_time(keyword | map, atom()) :: keyword | map
  def maybe_convert_date_time(options, key) when is_map(options) do
    case options do
      %{^key => %DateTime{} = date_time} ->
        timestamp = DateTime.to_iso8601(date_time)
        %{options | key => timestamp}

      _ ->
        options
    end
  end

  def maybe_convert_date_time(options, key) when is_list(options) do
    case Keyword.get(options, key) do
      %DateTime{} = date_time ->
        timestamp = DateTime.to_iso8601(date_time)
        Keyword.put(options, key, timestamp)

      _ ->
        options
    end
  end

  # If `:embed` is present, prepend to `:embeds` for compatibility
  def combine_embeds(%{embed: embed} = args),
    do: Map.delete(args, :embed) |> Map.put(:embeds, [embed | args[:embeds] || []])

  def combine_embeds(%{data: data} = args), do: %{args | data: combine_embeds(data)}
  def combine_embeds(%{message: data} = args), do: %{args | message: combine_embeds(data)}
  def combine_embeds(args), do: args

  # If `:file` is present, prepend to `:files` for compatibility
  def combine_files(%{file: file} = args),
    do: Map.delete(args, :file) |> Map.put(:files, [file | args[:files] || []])

  def combine_files(%{data: data} = args), do: %{args | data: combine_files(data)}
  def combine_files(%{message: data} = args), do: %{args | message: combine_files(data)}
  def combine_files(args), do: args

  def pop_files(%{data: data} = args),
    do: {data.files, %{args | data: Map.delete(data, :files)}}

  def pop_files(%{message: data} = args),
    do: {data.files, %{args | message: Map.delete(data, :files)}}

  def pop_files(args), do: Map.pop!(args, :files)

  def generate_boundary do
    String.duplicate("-", 20) <>
      "KraigieNostrumCat_" <>
      Base.encode16(:crypto.strong_rand_bytes(10))
  end

  def prepare_allowed_mentions(options) do
    with raw_options when raw_options != :all <- Map.get(options, :allowed_mentions, :all),
         allowed_mentions when is_map(allowed_mentions) <- parse_allowed_mentions(raw_options) do
      Map.put(options, :allowed_mentions, allowed_mentions)
    else
      _ ->
        Map.delete(options, :allowed_mentions)
    end
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
