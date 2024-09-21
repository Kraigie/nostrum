defmodule Nostrum.Voice.Crypto do
  @moduledoc false

  alias Nostrum.Struct.VoiceState
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Crypto.Aes
  alias Nostrum.Voice.Crypto.Chacha
  alias Nostrum.Voice.Crypto.Salsa

  @type cipher_rtpsize ::
          :xsalsa20_poly1305_lite_rtpsize
          | :aead_xchacha20_poly1305_rtpsize
          | :aead_aes256_gcm_rtpsize

  @type cipher_alias :: :aes256_gcm | :xchacha20_poly1305

  @type cipher_non_rtpsize ::
          :xsalsa20_poly1305
          | :xsalsa20_poly1305_suffix
          | :xsalsa20_poly1305_lite
          | :aead_aes256_gcm

  @type cipher :: cipher_non_rtpsize() | cipher_alias() | cipher_rtpsize()

  @fallback_mode :aead_xchacha20_poly1305_rtpsize

  @mode_aliases %{
    xchacha20_poly1305: :aead_xchacha20_poly1305_rtpsize,
    aes256_gcm: :aead_aes256_gcm_rtpsize
  }

  @spec encryption_mode(list(String.t())) :: cipher()
  def encryption_mode(available_modes) do
    mode = Application.get_env(:nostrum, :voice_encryption_mode, :aes256_gcm)

    mode = Map.get(@mode_aliases, mode, mode)

    if "#{mode}" in available_modes do
      mode
    else
      @fallback_mode
    end
  end

  def encrypt(%VoiceState{encryption_mode: mode} = voice, data) do
    header = Audio.rtp_header(voice)
    apply(__MODULE__, :"encrypt_#{mode}", [voice, data, header])
  end

  def decrypt(%{secret_key: key, encryption_mode: mode}, data) do
    apply(__MODULE__, :"decrypt_#{mode}", [key, data])
  end

  def encrypt_xsalsa20_poly1305(%VoiceState{secret_key: key}, data, header) do
    nonce = header <> <<0::unit(8)-size(12)>>

    [header, Salsa.encrypt(data, key, nonce)]
  end

  def encrypt_xsalsa20_poly1305_suffix(%VoiceState{secret_key: key}, data, header) do
    nonce = :crypto.strong_rand_bytes(24)

    [header, Salsa.encrypt(data, key, nonce), nonce]
  end

  def encrypt_xsalsa20_poly1305_lite(%VoiceState{secret_key: key} = voice, data, header) do
    {unpadded_nonce, nonce} = lite_nonce(voice)

    [header, Salsa.encrypt(data, key, nonce), unpadded_nonce]
  end

  def encrypt_xsalsa20_poly1305_lite_rtpsize(voice, data, header),
    do: encrypt_xsalsa20_poly1305_lite(voice, data, header)

  def encrypt_xchacha20_poly1305(voice, data, header),
    do: encrypt_aead_xchacha20_poly1305_rtpsize(voice, data, header)

  def encrypt_aead_xchacha20_poly1305_rtpsize(%VoiceState{secret_key: key} = voice, data, header) do
    {unpadded_nonce, nonce} = lite_nonce(voice)

    [header, Chacha.encrypt(data, key, nonce, _aad = header), unpadded_nonce]
  end

  def encrypt_aead_aes256_gcm(voice, data, header), do: encrypt_aes256_gcm(voice, data, header)

  def encrypt_aead_aes256_gcm_rtpsize(voice, data, header),
    do: encrypt_aes256_gcm(voice, data, header)

  def encrypt_aes256_gcm(%VoiceState{secret_key: key} = voice, data, header) do
    {unpadded_nonce, nonce} = lite_nonce(voice, 12)

    [header, Aes.encrypt(data, key, nonce, _aad = header), unpadded_nonce]
  end

  def decrypt_xsalsa20_poly1305(key, <<header::bytes-size(12), cipher_text::binary>>) do
    nonce = header <> <<0::unit(8)-size(12)>>

    Salsa.decrypt(cipher_text, key, nonce)
  end

  def decrypt_xsalsa20_poly1305_lite(key, data) do
    {_header, cipher_text, _tag = <<>>, nonce} = decode_packet(data, 4, 24, 0)

    Salsa.decrypt(cipher_text, key, nonce)
  end

  def decrypt_xsalsa20_poly1305_suffix(key, data) do
    {_header, cipher_text, _tag = <<>>, nonce} = decode_packet(data, 24, 24, 0)

    Salsa.decrypt(cipher_text, key, nonce)
  end

  def decrypt_xsalsa20_poly1305_lite_rtpsize(key, data) do
    {_header, cipher_text, _tag, nonce, ext_len} = decode_packet_rtpsize(data, 24, 0)

    <<_exts::unit(32)-size(ext_len), opus::binary>> = Salsa.decrypt(cipher_text, key, nonce)

    opus
  end

  def decrypt_xchacha20_poly1305(key, data),
    do: decrypt_aead_xchacha20_poly1305_rtpsize(key, data)

  def decrypt_aead_xchacha20_poly1305_rtpsize(key, data) do
    {header, cipher_text, tag, nonce, ext_len} = decode_packet_rtpsize(data, 24, 16)

    <<_exts::unit(32)-size(ext_len), opus::binary>> =
      Chacha.decrypt(cipher_text, key, nonce, _aad = header, tag)

    opus
  end

  def decrypt_aes256_gcm(key, data), do: decrypt_aead_aes256_gcm_rtpsize(key, data)

  def decrypt_aead_aes256_gcm_rtpsize(key, data) do
    {header, cipher_text, tag, nonce, ext_len} = decode_packet_rtpsize(data, 12, 16)

    <<_exts::unit(32)-size(ext_len), opus::binary>> =
      Aes.decrypt(cipher_text, key, nonce, _aad = header, tag)

    opus
  end

  def decrypt_aead_aes256_gcm(key, data) do
    {header, cipher_text, tag, nonce} = decode_packet(data, 4, 12, 16)

    Aes.decrypt(cipher_text, key, nonce, _aad = header, tag)
  end

  @lite_nonce_length 4

  defp lite_nonce(%VoiceState{rtp_sequence: rtp_sequence}, nonce_length \\ 24) do
    unpadded_nonce = <<rtp_sequence::32>>
    nonce = unpadded_nonce <> <<0::unit(8)-size(nonce_length - @lite_nonce_length)>>
    {unpadded_nonce, nonce}
  end

  # Discord's newer encryption modes ending in '_rtpsize' leave the first 4 bytes of the RTP
  # header extension in plaintext while encrypting the elements themselves. The AAD is the
  # 12-byte RTP header concatenated with the first 4 bytes of the RTP header extension.

  # Much like is done within the function `Nostrum.Voice.Opus.strip_rtp_ext/1`, we pattern match
  # on the `0xBEDE` constant and the 16-bit big-endian extension length that denotes the length
  # in 32-bit words of the extension elements. Because the elements are a part of the cipher text,
  # the extension length is the number of 32-bit words to discard after decryption to obtain
  # solely the opus packet.

  # This function returns a 5-element tuple with
  # - RTP header
  #   - Fixed 12 byte header concatenated with the first 4 bytes of the extension
  #   - Used as the AAD for AEAD ciphers
  # - cipher text
  #   - RTP extension elements prepended to the opus packet
  # - cipher tag (MAC)
  # - nonce (padded)
  # - RTP header extension length
  #   - for isolating the opus after decryption
  defp decode_packet_rtpsize(
         <<header::bytes-size(12), 0xBE, 0xDE, ext_len::integer-16, rest::binary>>,
         nonce_length,
         tag_length
       )
       when byte_size(rest) - (@lite_nonce_length + tag_length) > ext_len * 4 do
    header = header <> <<0xBE, 0xDE, ext_len::integer-16>>

    {cipher_text, tag, unpadded_nonce} = split_data(rest, @lite_nonce_length, tag_length)

    nonce = unpadded_nonce <> <<0::unit(8)-size(nonce_length - @lite_nonce_length)>>

    {header, cipher_text, tag, nonce, ext_len}
  end

  # Non "rtpsize" modes where everything is encrypted beyond the 12-byte header
  defp decode_packet(
         <<header::bytes-size(12), rest::binary>>,
         unpadded_nonce_length,
         nonce_length,
         tag_length
       ) do
    {cipher_text, tag, unpadded_nonce} = split_data(rest, unpadded_nonce_length, tag_length)

    nonce = unpadded_nonce <> <<0::unit(8)-size(nonce_length - unpadded_nonce_length)>>

    {header, cipher_text, tag, nonce}
  end

  defp split_data(data, unpadded_nonce_length, tag_length) do
    cipher_text_length = byte_size(data) - (unpadded_nonce_length + tag_length)

    <<cipher_text::bytes-size(cipher_text_length), tag::bytes-size(tag_length),
      unpadded_nonce::bytes-size(unpadded_nonce_length)>> = data

    {cipher_text, tag, unpadded_nonce}
  end
end
