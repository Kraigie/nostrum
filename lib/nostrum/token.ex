defmodule Nostrum.Token do
  @moduledoc """
  A helper module for verifying the Discord bot token.

  The token can be generated in the "Bot" tab of your Application the [Discord Developer Portal](https://discord.com/developers/applications) and
  can be configured in your config file.

  ```elixir
  config :nostrum,
    token: "666" # The token of your bot as a string
  ```
  """

  @invalid_token_error_message ~S[Invalid token format. Copy it again from the "Bot" tab of your Application in the Discord Developer Portal.]
  @no_token_error_message "A bot token needs to be supplied in your config file"

  @doc """
  Decode the given Discord bot token.

  We check if the token is a binary followed by splitting it into 3 parts separated by a dot `"."`.
  The first part is the Base64 encoded user_id which we decode and parse into as integer.
  The second part is an encoded timestamp, and the last part an arbitrary cryptographic signature.

  Returns the user ID in the token.

  Raises on failure.

  ## Examples

      iex> token = "OTY4NTU2MzQ4MzkwMzkxODU5.G49NjP.pD8PLpKp-Xx8sr-8m1DCxSPTJZdcpcJZOExc1c"
      iex> Nostrum.Token.decode_token!(token)
      968556348390391859

      iex> token = "ODY4MDcxODUzMDMyMzU3OTc4.YPqU6Q.jNJcq1daGG3otexX3c1LcxCpgpQ"
      iex> Nostrum.Token.decode_token!(token)
      868071853032357978
  """
  @doc since: "0.11.0"
  def decode_token!(nil), do: raise(@no_token_error_message)

  @spec decode_token!(binary()) :: pos_integer()
  def decode_token!(<<user_id::binary-size(24), 46, _ts::binary-size(6), 46, _hmac_auth::binary>>) do
    decode_user_id!(user_id)
  end

  def decode_token!(token) when is_binary(token) do
    case String.split(token, ".") do
      [user_id, _timestamp, _hmac_auth] -> decode_user_id!(user_id)
      _ -> raise(@invalid_token_error_message)
    end
  end

  defp decode_user_id!(user_id) do
    user_id
    |> Base.decode64!(padding: false)
    |> String.to_integer()
  rescue
    exception ->
      reraise(
        RuntimeError,
        [message: @invalid_token_error_message, exception: exception],
        __STACKTRACE__
      )
  end
end
