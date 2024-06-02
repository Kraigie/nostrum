defmodule Nostrum.Voice.Crypto.Chacha do
  @moduledoc """
  Handles encryption and decryption of outgoing and incoming voice data when a ChaCha encryption mode is selected

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and curious cryptographic cats.

  ### Purpose

  Erlang's `:crypto` module supports the chacha20_poly1305 AEAD stream cipher.
  Analogously to Salsa20 and XSalsa20, XChaCha20 is a way to use 192-bit nonces
  with ChaCha20 by hashing the key and part of the extended nonce to generate a
  sub-key, which is used as the input key for ChaCha20.

  To leverage the crypto module, we had to implement the HChaCha20 hash function
  in elixir to then pass the resulting sub-key to the `crypto_one_time_aead`.

  ### Implementation

  The HChaCha20 function takes the first 16-bytes of the extended 24-byte XChaCha20 nonce,
  expands the key and the 16-byte nonce slice into a block in place of the block count and
  usual smaller nonce. That block has 20 rounds of mutation, and instead of summing the block
  with its starting state as is done with keystream generation, 8 of the 16 bytes are taken
  and used as the sub-key, which is the input key for the standard chacha20 cipher.

  Even though we've implemented the bulk of what's needed to generate chacha20 key streams
  for encryption and decryption, we're only using this module to generate the inputs to
  use the :crypto module's chacha20_poly1305 functionality in the capacity of xchacha20
  as is required by Discord with that encryption mode selected.

  This is all in service of leveraging the performance benefits of the the NIF crypto
  functions, which are necessarily going to be more performant than anything implemented
  in pure elixir/erlang like the `:kcl` package.

  *ChaCha20 is a variant of the Salsa20 cipher. I will discuss in greater detail the implementation
  in the `Nostrum.Voice.Crypto.Salsa` module, where much is applicable here.*

  References for Salsa family of ciphers
  - https://cr.yp.to/snuffle/spec.pdf
  - https://cr.yp.to/chacha/chacha-20080128.pdf
  - https://cr.yp.to/snuffle/xsalsa-20110204.pdf
  - https://datatracker.ietf.org/doc/html/rfc7539
  - https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha

  ### Performance considerations

  After the XChaCha20 sub-key is generated in elixir, the crypto NIF function performs the
  heavy lifting. If your bot is running on a machine without AES hardware acceleration, you
  may find that using the chacha encryption mode yields better performance.
  """
  @moduledoc since: "0.10.0"

  import Bitwise

  import Nostrum.Voice.Crypto.Salsa, only: [block_binary_to_tuple: 1]

  @chacha_constant "expand 32-byte k"

  defp sum(a, b), do: a + b &&& 0xFFFFFFFF
  defp rotl(a, b), do: (a <<< b ||| a >>> (32 - b)) &&& 0xFFFFFFFF

  defp quarter_round(a, b, c, d) do
    a = a |> sum(b)
    d = d |> bxor(a) |> rotl(16)

    c = c |> sum(d)
    b = b |> bxor(c) |> rotl(12)

    a = a |> sum(b)
    d = d |> bxor(a) |> rotl(8)

    c = c |> sum(d)
    b = b |> bxor(c) |> rotl(7)

    {a, b, c, d}
  end

  defp quarter_round_on(tuple, index_a, index_b, index_c, index_d) do
    a = elem(tuple, index_a)
    b = elem(tuple, index_b)
    c = elem(tuple, index_c)
    d = elem(tuple, index_d)

    {a, b, c, d} = quarter_round(a, b, c, d)

    tuple
    |> put_elem(index_a, a)
    |> put_elem(index_b, b)
    |> put_elem(index_c, c)
    |> put_elem(index_d, d)
  end

  # Column round followed by diagonal round
  defp double_round(tuple) do
    tuple
    |> quarter_round_on(0, 4, 8, 12)
    |> quarter_round_on(1, 5, 9, 13)
    |> quarter_round_on(2, 6, 10, 14)
    |> quarter_round_on(3, 7, 11, 15)
    |> quarter_round_on(0, 5, 10, 15)
    |> quarter_round_on(1, 6, 11, 12)
    |> quarter_round_on(2, 7, 8, 13)
    |> quarter_round_on(3, 4, 9, 14)
  end

  defp twenty_rounds(block) do
    Enum.reduce(1..10, block, fn _, t -> double_round(t) end)
  end

  defp expand(<<key::bytes-32>> = _k, <<nonce::bytes-16>> = _n) do
    @chacha_constant <> key <> nonce
  end

  defp hchacha20(<<key::bytes-32>> = _k, <<first_sixteen::bytes-16, _last::bytes-8>> = _n) do
    key
    |> expand(first_sixteen)
    |> block_binary_to_tuple()
    |> twenty_rounds()
    |> hchacha20_block_tuple_to_binary()
  end

  defp xchacha20_key_and_nonce(<<key::bytes-32>> = _k, <<nonce::bytes-24>> = _n) do
    xchacha20_key = hchacha20(key, nonce)
    <<_first_sixteen::bytes-16, last_eight::bytes-8>> = nonce
    xchacha20_nonce = <<0, 0, 0, 0>> <> last_eight
    {xchacha20_key, xchacha20_nonce}
  end

  defp hchacha20_block_tuple_to_binary(
         {x0, x1, x2, x3, _, _, _, _, _, _, _, _, x12, x13, x14, x15}
       ) do
    <<x0::little-32, x1::little-32, x2::little-32, x3::little-32, x12::little-32, x13::little-32,
      x14::little-32, x15::little-32>>
  end

  @spec encrypt(binary(), <<_::256>>, <<_::192>>, binary()) :: iolist()
  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce, aad) do
    {xchacha_key, xchacha_nonce} = xchacha20_key_and_nonce(key, nonce)

    {cipher_text, tag} =
      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        xchacha_key,
        xchacha_nonce,
        plain_text,
        aad,
        _encrypt = true
      )

    [cipher_text, tag]
  end

  @spec decrypt(binary(), <<_::256>>, <<_::192>>, binary(), <<_::128>>) :: binary() | :error
  def decrypt(cipher_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce, aad, tag) do
    {xchacha_key, xchacha_nonce} = xchacha20_key_and_nonce(key, nonce)

    :crypto.crypto_one_time_aead(
      :chacha20_poly1305,
      xchacha_key,
      xchacha_nonce,
      cipher_text,
      aad,
      tag,
      _encrypt = false
    )
  end
end
