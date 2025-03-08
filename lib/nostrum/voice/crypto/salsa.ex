defmodule Nostrum.Voice.Crypto.Salsa do
  @moduledoc """
  Handles encryption and decryption of outgoing and incoming voice data when a Salsa encryption mode is selected

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and curious cryptographic cats.

  ### Purpose

  To support xsalsa20_poly1305 without a NIF, we had to implement the
  Salsa20 cipher and HSalsa20 hash function to use 192-bit nonces in the capacity
  of XSalsa20.

  Along with leveraging the :crypto module to perform the poly1305 MAC function
  and xor'ing arbitrary-length binaries, by being more thoughtful and explicit
  with our implementation, we should be able to eek out better performance
  than the `:kcl` package provides.

  ### Implementation

  > #### The salchicha package {: .neutral}
  >
  > The implementation of these ciphers has been moved into its own package for anyone
  > else wishing to use XSalsa20 and XChaCha20 encryption without a libsodium NIF.
  >
  > - Salchicha on [hex.pm](https://hex.pm/packages/salchicha)
  > - Salchicha on [github](https://github.com/BrandtHill/Salchicha)

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
  - Only supporting Salsa/ChaCha family ciphers, not full NaCl/libsodium API

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

  @spec encrypt(iodata(), <<_::256>>, <<_::192>>) :: iolist()
  def encrypt(plain_text, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce) do
    Salchicha.secretbox(plain_text, nonce, key)
  end

  @spec decrypt(iodata(), <<_::256>>, <<_::192>>) :: binary() | :error
  def decrypt(message, <<key::bytes-32>> = _key, <<nonce::bytes-24>> = _nonce) do
    with [_ | _] = plain_text <- Salchicha.secretbox_open(message, nonce, key) do
      IO.iodata_to_binary(plain_text)
    end
  end
end
