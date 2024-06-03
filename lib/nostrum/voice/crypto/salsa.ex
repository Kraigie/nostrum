defmodule Nostrum.Voice.Crypto.Salsa do
  @moduledoc """
  Handles encryption and decryption of outgoing and incoming voice data when a ChaCha encryption mode is selected

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and curious cryptographic cats.

  ### Purpose

  To support xsalsa20_poly1305 without a NIF, we have to implement the
  Salsa20 cipher and HSalsa20 hash function to use 192-bit nonces in the capacity
  of XSalsa20.

  Along with leveraging the :crypto module to perform the poly1305 MAC function
  and xor'ing arbitrary-length binaries, by being more thoughtful and explicit
  with our implementation, we should be able to eek out better performance
  than the `:kcl` package provides.

  ### Implementation

  The `:kcl` package is an impressive pure-elixir NaCl/libsodium compatible library
  that Nostrum previously used for voice encryption. For our usage with Discord voice,
  we only need the ability to encrypt and decrypt with 32-byte keys using 24-byte
  XSalsa20 nonces.

  Some of the key differences in our implementation compared to Kcl
  - Heavy use of explicit binary pattern matching instead of more traditional implicit enumeration
  - Intermediate block state stored in a 16-element tuple that is mutated during the 20-round hot loop instead of lists
  - Minimized the number binary copies, returning iolists when appropriate, instead of concatenating binaries
  - XOR whole keystream and message blocks instead of XOR'ing one byte at a time
  - Poly1305 MAC handled by crypto module instead of implemented in elixir
  - Only support 32-byte keys and 24-byte nonces (XSalsa20) instead of full NaCl/libsodium

  Additionally there appears to be a bug in how the 16-byte block count is serialized during key expansion:
  It's supposed to be little endian, and it happens to be for blocks 0-255, but for larger block counts,
  Kcl may become incompatible with NaCl/libsodium-type libraries. For Discord's use case of encrypting short
  20 millisecond compressed audio packets, block counts were well-below this suspected problem threshold.

  The cipher functions were implemented in the order they're defined in the original Salsa specification,
  and though it's using a lot of explicit binary pattern matching, it turned out to be quite legible.
  In a single statement of binary pattern matching, the 512-bit initial block state is cast into 16
  little-endian 32-bit words. Standard elixir patterns might have you iterate through the binary until the
  end was reached, but matching and casting all sixteen block elements in a single statement then returning
  a tuple is explicit, clear, and simple to understand when referenced against the spec.

  Readers interested in cryptography are encouraged to read more about the Salsa20/ChaCha20 ciphers.

  References for Salsa family of ciphers
  - https://cr.yp.to/snuffle/spec.pdf
  - https://cr.yp.to/chacha/chacha-20080128.pdf
  - https://cr.yp.to/snuffle/xsalsa-20110204.pdf
  - https://datatracker.ietf.org/doc/html/rfc7539
  - https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha

  ### Performance considerations

  The entire keystream generation and xor'ing the message with the stream is done in elixir,
  only performing the Poly1305 MAC function through the crypto module. Although it was implemented
  as thoughtfully and explicitly as possible with memory usage and performance in mind, using any
  of the Salsa modes will likely be less performant than ChaCha or AES.
  """
  @moduledoc since: "0.10.0"

  import Bitwise

  @salsa_constant "expand 32-byte k"
                  |> :binary.bin_to_list()
                  |> Enum.chunk_every(4)
                  |> Enum.map(&:binary.list_to_bin/1)

  defp sum(a, b), do: a + b &&& 0xFFFFFFFF
  defp rotl(a, b), do: (a <<< b ||| a >>> (32 - b)) &&& 0xFFFFFFFF

  defp quarter_round(a, b, c, d) do
    b = a |> sum(d) |> rotl(7) |> bxor(b)
    c = b |> sum(a) |> rotl(9) |> bxor(c)
    d = c |> sum(b) |> rotl(13) |> bxor(d)
    a = d |> sum(c) |> rotl(18) |> bxor(a)

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

  # Column round followed by row round
  defp double_round(tuple) do
    tuple
    |> quarter_round_on(0, 4, 8, 12)
    |> quarter_round_on(5, 9, 13, 1)
    |> quarter_round_on(10, 14, 2, 6)
    |> quarter_round_on(15, 3, 7, 11)
    |> quarter_round_on(0, 1, 2, 3)
    |> quarter_round_on(5, 6, 7, 4)
    |> quarter_round_on(10, 11, 8, 9)
    |> quarter_round_on(15, 12, 13, 14)
  end

  defp twenty_rounds(block) do
    Enum.reduce(1..10, block, fn _, t -> double_round(t) end)
  end

  defp expand(<<key::bytes-32>>, <<nonce::bytes-8>>, block_count) when is_integer(block_count) do
    # Full input is 64-bit nonce concatenated with little endian block count
    input = nonce <> <<block_count::little-64>>
    expand(key, input)
  end

  defp expand(<<k0::bytes-16, k1::bytes-16>> = _key, <<input::bytes-16>>) do
    [c0, c1, c2, c3] = @salsa_constant

    c0 <> k0 <> c1 <> input <> c2 <> k1 <> c3
  end

  defp hsalsa20(<<key::bytes-32>> = _k, <<first_sixteen::bytes-16, _last::bytes-8>> = _n) do
    key
    |> expand(first_sixteen)
    |> block_binary_to_tuple()
    |> twenty_rounds()
    |> hsalsa20_block_tuple_to_binary()
  end

  defp xsalsa20_key_and_nonce(<<key::bytes-32>> = _k, <<nonce::bytes-24>> = _n) do
    xsalsa20_key = hsalsa20(key, nonce)
    <<_first_sixteen::bytes-16, xsalsa20_nonce::bytes-8>> = nonce
    {xsalsa20_key, xsalsa20_nonce}
  end

  def block_binary_to_tuple(
        <<x0::little-32, x1::little-32, x2::little-32, x3::little-32, x4::little-32,
          x5::little-32, x6::little-32, x7::little-32, x8::little-32, x9::little-32,
          x10::little-32, x11::little-32, x12::little-32, x13::little-32, x14::little-32,
          x15::little-32>>
      ) do
    {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15}
  end

  defp hsalsa20_block_tuple_to_binary({x0, _, _, _, _, x5, x6, x7, x8, x9, x10, _, _, _, _, x15}) do
    <<x0::little-32, x5::little-32, x10::little-32, x15::little-32, x6::little-32, x7::little-32,
      x8::little-32, x9::little-32>>
  end

  defp sum_blocks(
         {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15},
         {y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15}
       ) do
    <<sum(x0, y0)::little-32, sum(x1, y1)::little-32, sum(x2, y2)::little-32,
      sum(x3, y3)::little-32, sum(x4, y4)::little-32, sum(x5, y5)::little-32,
      sum(x6, y6)::little-32, sum(x7, y7)::little-32, sum(x8, y8)::little-32,
      sum(x9, y9)::little-32, sum(x10, y10)::little-32, sum(x11, y11)::little-32,
      sum(x12, y12)::little-32, sum(x13, y13)::little-32, sum(x14, y14)::little-32,
      sum(x15, y15)::little-32>>
  end

  defp bxor_block(<<keystream::bytes-64>>, <<message::bytes-64>>) do
    :crypto.exor(keystream, message)
  end

  defp bxor_block(<<keystream::bytes-64>>, message) when byte_size(message) < 64 do
    keystream
    |> binary_part(0, byte_size(message))
    |> :crypto.exor(message)
  end

  defp keystream_block(key, nonce, block_count) do
    block =
      key
      |> expand(nonce, block_count)
      |> block_binary_to_tuple()

    block
    |> twenty_rounds()
    |> sum_blocks(block)
  end

  defp crypt(key, nonce, message, block_count \\ 0, outputs \\ [])

  defp crypt(key, nonce, <<message::bytes-64, rest::binary>>, block_count, outputs) do
    crypt(key, nonce, {message, rest}, block_count, outputs)
  end

  defp crypt(key, nonce, {<<message::bytes-64>>, <<rest::binary>>}, block_count, outputs) do
    output_block = crypt_block(key, nonce, message, block_count)
    crypt(key, nonce, rest, block_count + 1, [output_block | outputs])
  end

  defp crypt(key, nonce, <<message::binary>>, block_count, outputs) do
    output_block = crypt_block(key, nonce, message, block_count)
    _final_outputs = Enum.reverse([output_block | outputs])
  end

  defp crypt_block(key, nonce, message, block_count) do
    keystream = keystream_block(key, nonce, block_count)
    bxor_block(keystream, message)
  end

  # NaCl/libsodium use the first 32-bytes of the first Salsa20 keystream block as the OTP
  # for the Poly1305 MAC function. This is accomplished by prepending the messages with 32
  # zeros, so the XOR'ing yields just the keystream for those bytes. Only concatenate the
  # zeros with the first half block of the message to avoid copying entire message binary.
  defp prepare_message(<<first_thirty_two::bytes-32, rest::binary>>) when byte_size(rest) > 0 do
    {<<0::unit(8)-size(32), first_thirty_two::bytes-32>>, rest}
  end

  defp prepare_message(<<whole_message::binary>>) do
    <<0::unit(8)-size(32), whole_message::binary>>
  end

  @spec encrypt(binary(), <<_::256>>, <<_::192>>) :: iolist()
  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce) do
    {xsalsa_key, xsalsa_nonce} = xsalsa20_key_and_nonce(key, nonce)

    message = prepare_message(plain_text)

    # First block is guaranteed to be at least 32 bytes
    [<<mac_otp::bytes-32, cipher_text_head::binary>> | cipher_text_tail] =
      crypt(xsalsa_key, xsalsa_nonce, message)

    cipher_text = [cipher_text_head | cipher_text_tail]

    cipher_tag = :crypto.mac(:poly1305, mac_otp, cipher_text)

    [cipher_tag | cipher_text]
  end

  @spec decrypt(binary(), <<_::256>>, <<_::192>>) :: binary() | :error
  def decrypt(
        <<cipher_tag::bytes-16, cipher_text::binary>> = _encrypted_message,
        <<key::bytes-32>> = _key,
        <<nonce::bytes-24>> = _nonce
      ) do
    {xsalsa_key, xsalsa_nonce} = xsalsa20_key_and_nonce(key, nonce)

    message = prepare_message(cipher_text)

    # First block is guaranteed to be at least 32 bytes
    [<<mac_otp::bytes-32, plain_text_head::binary>> | plain_text_tail] =
      crypt(xsalsa_key, xsalsa_nonce, message)

    case :crypto.mac(:poly1305, mac_otp, cipher_text) do
      ^cipher_tag -> IO.iodata_to_binary([plain_text_head | plain_text_tail])
      _error -> :error
    end
  end
end
