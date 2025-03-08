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

  > #### The salchicha package {: .neutral}
  >
  > The implementation of these ciphers has been moved into its own package for anyone
  > else wishing to use XSalsa20 and XChaCha20 encryption without a libsodium NIF.
  >
  > - Salchicha on [hex.pm](https://hex.pm/packages/salchicha)
  > - Salchicha on [github](https://github.com/BrandtHill/Salchicha)

  The HChaCha20 function takes the first 16-bytes of the extended 24-byte XChaCha20 nonce,
  expands the key and the 16-byte nonce slice into a block in place of the block count and
  usual smaller nonce. That block has 20 rounds of mutation, and instead of summing the block
  with its starting state as is done with keystream generation, 8 of the 16 words are taken
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

  @spec encrypt(binary(), <<_::256>>, <<_::192>>, binary()) :: iolist()
  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce, aad) do
    Salchicha.xchacha20_poly1305_encrypt(plain_text, nonce, key, aad)
  end

  @spec decrypt(binary(), <<_::256>>, <<_::192>>, binary(), <<_::128>>) :: binary() | :error
  def decrypt(cipher_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce, aad, tag) do
    Salchicha.xchacha20_poly1305_decrypt_detached(cipher_text, nonce, key, aad, tag)
  end
end
