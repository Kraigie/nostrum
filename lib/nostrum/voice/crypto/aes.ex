defmodule Nostrum.Voice.Crypto.Aes do
  @moduledoc false

  # AES is fully supported by the erlang crypto module, so this module is
  # just to provide a convenient wrapper around it for encryption and decryption

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
