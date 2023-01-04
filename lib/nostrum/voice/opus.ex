defmodule Nostrum.Voice.Opus do
  @moduledoc false

  import Bitwise

  # Number of samples in a 20ms Opus frame at 48kHz sample rate
  @samples_per_frame 960

  # Number of microseconds per Opus frame/packet
  @usec_per_frame 20_000

  def samples_per_frame, do: @samples_per_frame

  def usec_per_frame, do: @usec_per_frame

  @doc """
  Strips the RTP header extension from an RTP payload.

  The encrypted portion of the RTP packets that we receive from Discord
  isn't raw opus packets; it's actually opus prepended by an RTP header extension.
  Looking at the unencrypted fixed-length RTP header, the extension bit is, in
  fact, set to 1.

  RFC 8285 describes the format of RTP header extensions. Discord uses RTP
  extensions with the one-byte header format that begins with a 0xBEDE pattern
  followed by a 16 bit extension length. This extension length represents the length
  of the following extension in 32-bit words.

  Because the RTP header extension elements don't necessarily mean anything to the client
  and Discord does not document what their extension element ids and corresponding values
  mean, they can easily be skipped over since the total extension length is provided at
  the beginning of the extension.
  """
  def strip_rtp_ext(
        <<
          0xBE,
          0xDE,
          ext_len::integer-16,
          _exts::unit(32)-size(ext_len),
          rest::binary
        >> = _packet
      ),
      do: rest

  def strip_rtp_ext(packet), do: packet

  def parse_ogg(<<>>), do: []

  def parse_ogg(binary) do
    %{rest: rest, body: body} = page = parse_ogg_page(binary)

    body =
      case body do
        <<"OpusHead", _::binary>> ->
          parse_opus_head(body)

        <<"OpusTags", _::binary>> ->
          parse_opus_tags(body)

        _ ->
          body
      end

    page = Map.delete(page, :rest)
    page = Map.put(page, :body, body)

    [page | parse_ogg(rest)]
  end

  def create_ogg_bitstream(opus_packets) do
    <<bitstream_serial::little-integer-32>> = :crypto.strong_rand_bytes(4)

    state = %{
      page_sequence: 0,
      granule_position: 0,
      header_type: 2,
      bitstream_serial: bitstream_serial
    }

    head =
      %{
        version: 1,
        channel_count: 2,
        pre_skip: 0,
        input_sample_rate: 48_000,
        output_gain: 0,
        mapping_family: 0
      }
      |> encode_opus_head()
      |> gen_page(state)

    state = %{state | header_type: 0, page_sequence: state.page_sequence + 1}

    tags =
      %{
        vendor_string: "Discord",
        comments: ["encoder=Discord"]
      }
      |> encode_opus_tags()
      |> gen_page(state)

    {opus_pages, state} =
      opus_packets
      |> Enum.chunk_every(50)
      |> Enum.map_reduce(state, fn chunk, state ->
        state = %{
          state
          | page_sequence: state.page_sequence + 1,
            granule_position: state.granule_position + @samples_per_frame * length(chunk)
        }

        {gen_page(chunk, state), state}
      end)

    eos =
      gen_page([], %{
        state
        | page_sequence: state.page_sequence + 1,
          header_type: 4
      })

    [head, tags | opus_pages ++ [eos]]
  end

  def gen_page(body, state) when not is_list(body), do: gen_page([body], state)

  def gen_page(
        body,
        %{
          page_sequence: page_sequence,
          bitstream_serial: bitstream_serial,
          granule_position: granule_position,
          header_type: header_type
        } = _state
      )
      when is_list(body) do
    segment_table_raw =
      body
      |> Enum.map(&byte_size/1)
      |> encode_ogg_segment_table()

    %{
      version: 0,
      header_type: header_type,
      granule_position: granule_position,
      bitstream_serial: bitstream_serial,
      page_sequence: page_sequence,
      crc_checksum: 0,
      page_segments: byte_size(segment_table_raw),
      segment_table_raw: segment_table_raw,
      body: :binary.list_to_bin(body)
    }
    |> encode_with_crc()
  end

  def parse_opus_head(
        <<
          "OpusHead",
          version::integer-8,
          channel_count::integer-8,
          pre_skip::little-integer-16,
          input_sample_rate::little-integer-32,
          output_gain::little-integer-16,
          mapping_family::integer-8,
          rest::binary
        >> = _binary
      ) do
    %{
      version: version,
      channel_count: channel_count,
      pre_skip: pre_skip,
      input_sample_rate: input_sample_rate,
      output_gain: output_gain,
      mapping_family: mapping_family,
      rest: rest
    }
  end

  def encode_opus_head(
        %{
          version: version,
          channel_count: channel_count,
          pre_skip: pre_skip,
          input_sample_rate: input_sample_rate,
          output_gain: output_gain,
          mapping_family: mapping_family
        } = _head
      ) do
    <<
      "OpusHead",
      version::integer-8,
      channel_count::integer-8,
      pre_skip::little-integer-16,
      input_sample_rate::little-integer-32,
      output_gain::little-integer-16,
      mapping_family::integer-8
    >>
  end

  def parse_opus_tags(
        <<
          "OpusTags",
          vendor_string_length::little-integer-32,
          vendor_string::binary-size(vendor_string_length),
          user_comment_list_length::little-integer-32,
          rest::binary
        >> = _binary
      ) do
    {rest, comments} =
      if user_comment_list_length > 0 do
        Enum.reduce(1..user_comment_list_length, {rest, []}, fn _, {rest, list} ->
          <<
            comment_length::little-integer-32,
            comment::binary-size(comment_length),
            rest::binary
          >> = rest

          {rest, [comment | list]}
        end)
      else
        {rest, []}
      end

    comments = Enum.reverse(comments)

    %{
      vendor_string: vendor_string,
      comments: comments,
      rest: rest
    }
  end

  def encode_opus_tags(
        %{
          vendor_string: vendor_string,
          comments: comments
        } = _tags
      ) do
    comments_binary =
      Enum.reduce(comments, <<>>, fn comment, acc ->
        len = byte_size(comment)
        acc <> <<len::little-integer-32, comment::binary>>
      end)

    <<
      "OpusTags",
      byte_size(vendor_string)::little-integer-32,
      vendor_string::binary,
      Enum.count(comments)::little-integer-32,
      comments_binary::binary
    >>
  end

  def parse_ogg_page(
        <<
          "OggS",
          version::integer-8,
          header_type::integer-8,
          granule_position::little-integer-64,
          bitstream_serial::little-integer-32,
          page_sequence::little-integer-32,
          crc_checksum::little-integer-32,
          page_segments::integer-8,
          segment_table_raw::binary-size(page_segments),
          rest::binary
        >> = _binary
      ) do
    segment_table = parse_ogg_segment_table(segment_table_raw)

    body_len = segment_table |> Enum.sum()

    <<body::binary-size(body_len), rest::binary>> = rest

    %{
      version: version,
      header_type: header_type,
      granule_position: granule_position,
      bitstream_serial: bitstream_serial,
      page_sequence: page_sequence,
      crc_checksum: crc_checksum,
      page_segments: page_segments,
      segment_table: segment_table,
      segment_table_raw: segment_table_raw,
      body: body,
      rest: rest
    }
  end

  def encode_ogg_page(
        %{
          version: version,
          header_type: header_type,
          granule_position: granule_position,
          bitstream_serial: bitstream_serial,
          page_sequence: page_sequence,
          crc_checksum: crc_checksum,
          page_segments: page_segments,
          segment_table_raw: segment_table_raw,
          body: body
        } = _page
      ) do
    <<
      "OggS",
      version::integer-8,
      header_type::integer-8,
      granule_position::little-integer-64,
      bitstream_serial::little-integer-32,
      page_sequence::little-integer-32,
      crc_checksum::little-integer-32,
      page_segments::integer-8,
      segment_table_raw::binary,
      body::binary
    >>
  end

  def parse_ogg_segment_table(table) do
    table
    |> :binary.bin_to_list()
    |> Enum.reduce([0], fn seg, [head | tail] ->
      if rem(head, 255) == 0 and seg != 0,
        do: [head + seg | tail],
        else: [seg | [head | tail]]
    end)
    |> Enum.reverse()
  end

  def encode_ogg_segment_table(table) do
    table
    |> Enum.map(fn seg ->
      if seg < 255,
        do: seg,
        else: [255, encode_ogg_segment_table([seg - 255])]
    end)
    |> :binary.list_to_bin()
  end

  def encode_with_crc(page_map) do
    crc_checksum =
      page_map
      |> encode_ogg_page()
      |> crc32()

    page_map
    |> Map.put(:crc_checksum, crc_checksum)
    |> encode_ogg_page()
  end

  @doc """
  Get the length of a gap between two consecutive RTP packets in 20ms frame lengths
  """
  def get_gap_length({{_, time, _}, _}, {{_, prev_time, _}, _}),
    do: get_gap_length(time, prev_time)

  def get_gap_length(time, prev_time) do
    diff_samples = time - prev_time
    diff_frames = div(diff_samples, @samples_per_frame)
    max(0, diff_frames - 1)
  end

  @opus_silence <<0xF8, 0xFF, 0xFE>>

  def generate_silence(num_frames), do: List.duplicate(@opus_silence, num_frames)

  @doc """
  Interleave silence in gaps found between consecutive RTP packets
  """
  def pad_opus(packets) do
    Enum.flat_map_reduce(packets, hd(packets), fn {_, opus} = packet, prev_packet ->
      get_gap_length(packet, prev_packet)
      |> generate_silence()
      |> (&(&1 ++ [opus])).()
      |> (&{&1, packet}).()
    end)
    |> elem(0)
  end

  @crc_lookup_table [
    0x00000000,
    0x04C11DB7,
    0x09823B6E,
    0x0D4326D9,
    0x130476DC,
    0x17C56B6B,
    0x1A864DB2,
    0x1E475005,
    0x2608EDB8,
    0x22C9F00F,
    0x2F8AD6D6,
    0x2B4BCB61,
    0x350C9B64,
    0x31CD86D3,
    0x3C8EA00A,
    0x384FBDBD,
    0x4C11DB70,
    0x48D0C6C7,
    0x4593E01E,
    0x4152FDA9,
    0x5F15ADAC,
    0x5BD4B01B,
    0x569796C2,
    0x52568B75,
    0x6A1936C8,
    0x6ED82B7F,
    0x639B0DA6,
    0x675A1011,
    0x791D4014,
    0x7DDC5DA3,
    0x709F7B7A,
    0x745E66CD,
    0x9823B6E0,
    0x9CE2AB57,
    0x91A18D8E,
    0x95609039,
    0x8B27C03C,
    0x8FE6DD8B,
    0x82A5FB52,
    0x8664E6E5,
    0xBE2B5B58,
    0xBAEA46EF,
    0xB7A96036,
    0xB3687D81,
    0xAD2F2D84,
    0xA9EE3033,
    0xA4AD16EA,
    0xA06C0B5D,
    0xD4326D90,
    0xD0F37027,
    0xDDB056FE,
    0xD9714B49,
    0xC7361B4C,
    0xC3F706FB,
    0xCEB42022,
    0xCA753D95,
    0xF23A8028,
    0xF6FB9D9F,
    0xFBB8BB46,
    0xFF79A6F1,
    0xE13EF6F4,
    0xE5FFEB43,
    0xE8BCCD9A,
    0xEC7DD02D,
    0x34867077,
    0x30476DC0,
    0x3D044B19,
    0x39C556AE,
    0x278206AB,
    0x23431B1C,
    0x2E003DC5,
    0x2AC12072,
    0x128E9DCF,
    0x164F8078,
    0x1B0CA6A1,
    0x1FCDBB16,
    0x018AEB13,
    0x054BF6A4,
    0x0808D07D,
    0x0CC9CDCA,
    0x7897AB07,
    0x7C56B6B0,
    0x71159069,
    0x75D48DDE,
    0x6B93DDDB,
    0x6F52C06C,
    0x6211E6B5,
    0x66D0FB02,
    0x5E9F46BF,
    0x5A5E5B08,
    0x571D7DD1,
    0x53DC6066,
    0x4D9B3063,
    0x495A2DD4,
    0x44190B0D,
    0x40D816BA,
    0xACA5C697,
    0xA864DB20,
    0xA527FDF9,
    0xA1E6E04E,
    0xBFA1B04B,
    0xBB60ADFC,
    0xB6238B25,
    0xB2E29692,
    0x8AAD2B2F,
    0x8E6C3698,
    0x832F1041,
    0x87EE0DF6,
    0x99A95DF3,
    0x9D684044,
    0x902B669D,
    0x94EA7B2A,
    0xE0B41DE7,
    0xE4750050,
    0xE9362689,
    0xEDF73B3E,
    0xF3B06B3B,
    0xF771768C,
    0xFA325055,
    0xFEF34DE2,
    0xC6BCF05F,
    0xC27DEDE8,
    0xCF3ECB31,
    0xCBFFD686,
    0xD5B88683,
    0xD1799B34,
    0xDC3ABDED,
    0xD8FBA05A,
    0x690CE0EE,
    0x6DCDFD59,
    0x608EDB80,
    0x644FC637,
    0x7A089632,
    0x7EC98B85,
    0x738AAD5C,
    0x774BB0EB,
    0x4F040D56,
    0x4BC510E1,
    0x46863638,
    0x42472B8F,
    0x5C007B8A,
    0x58C1663D,
    0x558240E4,
    0x51435D53,
    0x251D3B9E,
    0x21DC2629,
    0x2C9F00F0,
    0x285E1D47,
    0x36194D42,
    0x32D850F5,
    0x3F9B762C,
    0x3B5A6B9B,
    0x0315D626,
    0x07D4CB91,
    0x0A97ED48,
    0x0E56F0FF,
    0x1011A0FA,
    0x14D0BD4D,
    0x19939B94,
    0x1D528623,
    0xF12F560E,
    0xF5EE4BB9,
    0xF8AD6D60,
    0xFC6C70D7,
    0xE22B20D2,
    0xE6EA3D65,
    0xEBA91BBC,
    0xEF68060B,
    0xD727BBB6,
    0xD3E6A601,
    0xDEA580D8,
    0xDA649D6F,
    0xC423CD6A,
    0xC0E2D0DD,
    0xCDA1F604,
    0xC960EBB3,
    0xBD3E8D7E,
    0xB9FF90C9,
    0xB4BCB610,
    0xB07DABA7,
    0xAE3AFBA2,
    0xAAFBE615,
    0xA7B8C0CC,
    0xA379DD7B,
    0x9B3660C6,
    0x9FF77D71,
    0x92B45BA8,
    0x9675461F,
    0x8832161A,
    0x8CF30BAD,
    0x81B02D74,
    0x857130C3,
    0x5D8A9099,
    0x594B8D2E,
    0x5408ABF7,
    0x50C9B640,
    0x4E8EE645,
    0x4A4FFBF2,
    0x470CDD2B,
    0x43CDC09C,
    0x7B827D21,
    0x7F436096,
    0x7200464F,
    0x76C15BF8,
    0x68860BFD,
    0x6C47164A,
    0x61043093,
    0x65C52D24,
    0x119B4BE9,
    0x155A565E,
    0x18197087,
    0x1CD86D30,
    0x029F3D35,
    0x065E2082,
    0x0B1D065B,
    0x0FDC1BEC,
    0x3793A651,
    0x3352BBE6,
    0x3E119D3F,
    0x3AD08088,
    0x2497D08D,
    0x2056CD3A,
    0x2D15EBE3,
    0x29D4F654,
    0xC5A92679,
    0xC1683BCE,
    0xCC2B1D17,
    0xC8EA00A0,
    0xD6AD50A5,
    0xD26C4D12,
    0xDF2F6BCB,
    0xDBEE767C,
    0xE3A1CBC1,
    0xE760D676,
    0xEA23F0AF,
    0xEEE2ED18,
    0xF0A5BD1D,
    0xF464A0AA,
    0xF9278673,
    0xFDE69BC4,
    0x89B8FD09,
    0x8D79E0BE,
    0x803AC667,
    0x84FBDBD0,
    0x9ABC8BD5,
    0x9E7D9662,
    0x933EB0BB,
    0x97FFAD0C,
    0xAFB010B1,
    0xAB710D06,
    0xA6322BDF,
    0xA2F33668,
    0xBCB4666D,
    0xB8757BDA,
    0xB5365D03,
    0xB1F740B4
  ]

  for {val, i} <- Enum.with_index(@crc_lookup_table) do
    def lookup(unquote(i)), do: unquote(val)
  end

  def crc32(binary) do
    binary
    |> :binary.bin_to_list()
    |> Enum.reduce(0, fn byte, crc ->
      bxor(crc <<< 8 &&& 0xFFFFFFFF, lookup(bxor(crc >>> 24 &&& 0xFF, byte)))
    end)
  end
end
