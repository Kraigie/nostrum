defmodule Nostrum.Voice.Crypto.Aes do
  @moduledoc """
  Handles encryption and decryption of outgoing and incoming voice data when an AES encryption mode is selected

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and curious cryptographic cats.

  ### Purpose

  AES is fully supported by the erlang `:crypto` module, so this module
  just provides a convenient wrapper around it for encryption and decryption.

  ### Implementation

  Internally the functions `:crypto.crypto_one_time_aead/6` and `:crypto.crypto_one_time_aead/7` are
  called with the cipher argument set to `:aes_256_gcm`. Because these functions are implemented as
  NIFs with OpenSSL bindings, they faster than anything written in pure erlang/elixir.

  Because the crypto function returns a tuple with the cipher text and tag (MAC) separately, the return
  value is an iolist with them in the order that Discord expects. This is done to reduce binary copies
  as the functions ingesting the encrypted messages all support iodata.

  Readers are encouraged to read more about the [erlang crypto module](https://www.erlang.org/doc/man/crypto).

  ### Performance considerations

  AES is a well-established cipher and many modern CPUs have AES instruction sets to accelerate
  AES operations. While the Salsa20 and ChaCha20 stream ciphers are typically faster with pure
  software implementations, hardware acceleration will typically give AES the leg up for performance.
  """
  @moduledoc since: "0.10.0"

  @spec encrypt(binary(), <<_::256>>, <<_::96>>, binary()) :: iolist()
  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-12>> = _nonce, aad) do
    {cipher_text, tag} =
      :crypto.crypto_one_time_aead(:aes_256_gcm, key, nonce, plain_text, aad, _encrypt = true)

    [cipher_text, tag]
  end

  @spec decrypt(binary(), <<_::256>>, <<_::96>>, binary(), <<_::128>>) :: binary() | :error
  def decrypt(cipher_text, <<key::bytes-32>> = _key, <<nonce::bytes-12>> = _nonce, aad, tag) do
    :crypto.crypto_one_time_aead(
      :aes_256_gcm,
      key,
      nonce,
      cipher_text,
      aad,
      tag,
      _encrypt = false
    )
  end
end
