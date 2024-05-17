defmodule Nostrum.Voice.Crypto.Salsa do
  @moduledoc false

  # To support xsalsa20_poly1305 without a NIF, we have to implement the
  # Salsa20 cipher and HSalsa20 hash function to use 192-bit nonces.
  #
  # Along with leveraging the :crypto module to perform the poly1305 MAC function
  # and xor'ing arbitrary-length binaries, by being more thoughtful and explicit
  # with our implementation, we should be able to eek out better performance
  # than the `:kcl` package provides.
  #
  # References for Salsa family of ciphers
  # https://cr.yp.to/snuffle/spec.pdf
  # https://cr.yp.to/chacha/chacha-20080128.pdf
  # https://cr.yp.to/snuffle/xsalsa-20110204.pdf
  # https://datatracker.ietf.org/doc/html/rfc7539
  # https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-xchacha

  import Bitwise

  @salsa_constant "expand 32-byte k"
                  |> :binary.bin_to_list()
                  |> Enum.chunk_every(4)
                  |> Enum.map(&:binary.list_to_bin/1)

  defp sum(a, b), do: a + b &&& 0xFFFFFFFF
  defp rotl(a, b), do: (a <<< b ||| a >>> (32 - b)) &&& 0xFFFFFFFF

  def quarter_round(a, b, c, d) do
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

  def twenty_rounds(block) do
    Enum.reduce(1..10, block, fn _, t -> double_round(t) end)
  end

  def expand(<<key::bytes-32>>, <<nonce::bytes-8>>, block_count) when is_integer(block_count) do
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

  def xsalsa20_key_and_nonce(<<key::bytes-32>> = _k, <<nonce::bytes-24>> = _n) do
    xsalsa20_key = hsalsa20(key, nonce)
    <<_first_sixteen::bytes-16, xsalsa20_nonce::bytes-8>> = nonce
    {xsalsa20_key, xsalsa20_nonce}
  end

  defp block_binary_to_tuple(
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

  def bxor_block(<<keystream::bytes-64>>, <<message::bytes-64>>) do
    :crypto.exor(keystream, message)
  end

  def bxor_block(<<keystream::bytes-64>>, message) when byte_size(message) < 64 do
    keystream
    |> binary_part(0, byte_size(message))
    |> :crypto.exor(message)
  end

  def keystream_block(key, nonce, block_count) do
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
    output_block = crypt_block(key, nonce, message, block_count)
    crypt(key, nonce, rest, block_count + 1, [output_block | outputs])
  end

  defp crypt(key, nonce, <<message::binary>>, block_count, outputs) do
    output_block = crypt_block(key, nonce, message, block_count)
    outputs = Enum.reverse([output_block | outputs])
    IO.iodata_to_binary(outputs)
  end

  defp crypt_block(key, nonce, message, block_count) do
    keystream = keystream_block(key, nonce, block_count)
    bxor_block(keystream, message)
  end

  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce) do
    {xsalsa_key, xsalsa_nonce} = xsalsa20_key_and_nonce(key, nonce)
    message = <<0::unit(8)-size(32)>> <> plain_text
    <<mac_otp::bytes-32, cipher_text::binary>> = crypt(xsalsa_key, xsalsa_nonce, message)
    cipher_tag = :crypto.mac(:poly1305, mac_otp, cipher_text)
    cipher_tag <> cipher_text
  end

  def decrypt(
        <<cipher_tag::bytes-16, cipher_text::binary>> = _encrypted_message,
        <<key::bytes-32>>,
        <<nonce::bytes-24>>
      ) do
    {xsalsa_key, xsalsa_nonce} = xsalsa20_key_and_nonce(key, nonce)
    message = <<0::unit(8)-size(32)>> <> cipher_text
    <<mac_otp::bytes-32, plain_text::binary>> = crypt(xsalsa_key, xsalsa_nonce, message)

    case :crypto.mac(:poly1305, mac_otp, cipher_text) do
      ^cipher_tag -> plain_text
      _error -> :error
    end
  end
end
