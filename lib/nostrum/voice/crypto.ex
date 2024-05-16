defmodule Nostrum.Voice.Crypto do
  @moduledoc false

  alias Nostrum.Struct.VoiceState
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Crypto.Chacha

  @type cipher :: :xchacha20_poly1305 | :xsalsa20_poly1305 | :aes256_gcm

  @cipher Application.compile_env(:nostrum, :voice_encryption_mode, :aes256_gcm)

  @encryption_mode Map.get(
                     %{
                       xchacha20_poly1305: "aead_xchacha20_poly1305_rtpsize",
                       xsalsa20_poly1305: "xsalsa20_poly1305_lite_rtpsize",
                       aes256_gcm: "aead_aes256_gcm_rtpsize"
                     },
                     @cipher,
                     "aead_aes256_gcm_rtpsize"
                   )

  def encryption_mode, do: @encryption_mode

  def encrypt(voice, data) do
    apply(__MODULE__, :"encrypt_#{@cipher}", [voice, data])
  end

  def decrypt(voice, data) do
    apply(__MODULE__, :"decrypt_#{@cipher}", [voice, data])
  end

  def encrypt_xchacha20_poly1305(%VoiceState{secret_key: key, rtp_sequence: seq} = voice, data) do
    header = Audio.rtp_header(voice)

    unpadded_nonce = <<seq::32>>

    # 24 byte nonce
    nonce = unpadded_nonce <> <<0::unit(8)-size(20)>>

    {xchacha_key, xchacha_nonce} = Chacha.xchacha20_key_and_nonce(key, nonce)

    {cipher_text, tag} =
      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        xchacha_key,
        xchacha_nonce,
        data,
        _aad = header,
        _encrypt = true
      )

    header <> cipher_text <> tag <> unpadded_nonce
  end

  def decrypt_xchacha20_poly1305(%VoiceState{secret_key: key}, data) do
    {header, cipher_text, tag, nonce, ext_len} = decode_packet(data, 24)
    {xchacha_key, xchacha_nonce} = Chacha.xchacha20_key_and_nonce(key, nonce)

    <<_exts::unit(32)-size(ext_len), opus::binary>> =
      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        xchacha_key,
        xchacha_nonce,
        cipher_text,
        _aad = header,
        tag,
        _encrypt = false
      )

    opus
  end

  def encrypt_xsalsa20_poly1305(%VoiceState{secret_key: key, rtp_sequence: seq} = voice, data) do
    header = Audio.rtp_header(voice)

    unpadded_nonce = <<seq::32>>

    # 24 byte nonce
    nonce = unpadded_nonce <> <<0::unit(8)-size(20)>>

    header <> Kcl.secretbox(data, nonce, key) <> unpadded_nonce
  end

  def decrypt_xsalsa20_poly1305(%VoiceState{secret_key: key}, data) do
    {_header, cipher_text, _tag, nonce, ext_len} = decode_packet(data, 24, 0)
    <<_exts::unit(32)-size(ext_len), opus::binary>> = Kcl.secretunbox(cipher_text, nonce, key)
    opus
  end

  def encrypt_aes256_gcm(%VoiceState{secret_key: key, rtp_sequence: seq} = voice, data) do
    header = Audio.rtp_header(voice)

    unpadded_nonce = <<seq::32>>

    # 12 byte nonce
    nonce = unpadded_nonce <> <<0::unit(8)-size(8)>>

    {cipher_text, tag} =
      :crypto.crypto_one_time_aead(:aes_256_gcm, key, nonce, data, _aad = header, _encrypt = true)

    header <> cipher_text <> tag <> unpadded_nonce
  end

  def decrypt_aes256_gcm(%VoiceState{secret_key: key}, data) do
    {header, cipher_text, tag, nonce, ext_len} = decode_packet(data, 12)

    <<_exts::unit(32)-size(ext_len), opus::binary>> =
      :crypto.crypto_one_time_aead(
        :aes_256_gcm,
        key,
        nonce,
        cipher_text,
        _aad = header,
        tag,
        _encrypt = false
      )

    opus
  end

  @unpadded_nonce_length 4

  @doc """
  Discord's newer encryption modes ending in '_rtpsize' leave the first 4 bytes of the RTP
  header extension in plaintext while encrypting the elements themselves. The AAD is the
  12-byte RTP header concatenated with the first 4 bytes of the RTP header extension.

  Much like is done within the function `Nostrum.Voice.Opus.strip_rtp_ext/1`, we pattern match
  on the `0xBEDE` constant and the 16-bit big-endian extension length that denotes the length
  in 32-bit words of the extension elements. Because the elements are a part of the cipher text,
  the extension length is the number of 32-bit words to discard after decryption to obtain
  solely the opus packet.

  This function returns a 5-element tuple with
  - RTP header
    - Fixed 12 byte header concatenated with the first 4 bytes of the extension
    - Used as the AAD for AEAD ciphers
  - cipher text
    - RTP extension elements prepended to the opus packet
  - cipher tag (MAC)
  - nonce (padded)
  - RTP header extension length
    - for isolating the opus after decryption
  """
  def decode_packet(
        <<header::bytes-size(12), 0xBE, 0xDE, ext_len::integer-16, rest::binary>>,
        nonce_length \\ 24,
        tag_length \\ 16
      )
      when byte_size(rest) - (@unpadded_nonce_length + tag_length) > ext_len * 4 do
    header = header <> <<0xBE, 0xDE, ext_len::integer-16>>
    cipher_text_len = byte_size(rest) - (tag_length + @unpadded_nonce_length)

    <<cipher_text::bytes-size(cipher_text_len), tag::bytes-size(tag_length),
      unpadded_nonce::bytes-size(@unpadded_nonce_length)>> = rest

    nonce = unpadded_nonce <> <<0::unit(8)-size(nonce_length - @unpadded_nonce_length)>>

    {header, cipher_text, tag, nonce, ext_len}
  end
end
