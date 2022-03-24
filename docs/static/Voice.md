## Voice Channels
Discord voice channels allow audio data to be sent to the voice servers over UDP.
A bot is able to connect to up to one voice channel per guild. One websocket
connection will be opened and maintained for each voice channel the bot joins.
The websocket connection should reconnect automatically the same way that the 
main Discord gateway websocket connections do. For available voice functions and
usage see the `Nostrum.Voice` module.

## FFmpeg
Nostrum uses the powerful [ffmpeg](https://ffmpeg.org/) command line utility to
encode any audio (or video) file for sending to Discord's voice servers.
By default Nostrum will look for the executable `ffmpeg` in the system path.
If the executable is elsewhere, the path may be configured via
`config :nostrum, :ffmpeg, "/path/to/ffmpeg"`.
The function `Nostrum.Voice.play/4` allows sound to played via files, local or 
remote, or via raw data that gets piped to `stdin` of the `ffmpeg` process.
When playing from a url, the url can be a name of a file on the filesystem or a url
of file on a remote server - [ffmpeg supports a ton of protocols](https://www.ffmpeg.org/ffmpeg-protocols.html),
the most common of which are probably `http` or simply reading a file from the filesystem.
It is also possible to send raw opus frames, bypassing ffmpeg, if desired.

## youtube-dl
With only `ffmpeg` installed, Nostrum supports playing audio/video files or raw, piped
data as discussed in the section above. Nostrum also has support for `youtube-dl`, another
powerful command line utility for downloading audio/video from online video services.
Although the name implies support for Youtube, `youtube-dl` supports downloading from
[an immense list of sites](https://github.com/ytdl-org/youtube-dl/blob/master/docs/supportedsites.md).
By default Nostrum will look for the executable `youtube-dl` in the system path. If the
executable is elsewhere, the path may be configured via `config :nostrum, :youtubedl, "/path/to/youtube-dl"`.
When `Nostrum.Voice.play/4` is called with `:ytdl` for the `type` parameter, `youtube-dl` will be
run with options `-f bestaudio -q -o -`, which will attempt to download the audio at the given url and pipe it to `ffmpeg`.

## streamlink
Nostrum also has support for `streamlink`, yet another powerful command line utility 
for downloading live streams from online video streaming services.
By default Nostrum will look for the executable `streamlink` in the system path. 
If the executable is elsewhere, the path may be configured via `config :nostrum, :streamlink, "/path/to/streamlink"`.
When `Nostrum.Voice.play/4` is called with `:stream` for the `type` parameter, `streamlink` 
will attempt to download the live stream content and pipe it to `ffmpeg`.
It's recommended to use the most up-to-date version of `streamlink` to properly
play human-readable URLs from services such as Youtube and Twitch. Version 3.x.x
currently works with both of these services. If the short, human-readable url of the streaming service
doesn't work with `streamlink` out of the box, you may have more luck extracting the underlying raw stream url.
These are typically long URLs that end in `.m3u8` or `.hls`. If you have `youtube-dl` installed,
you can attempt to get this URL by running the following:

```elixir
{raw_url, 0} = System.cmd("youtube-dl", ["-f", "best", "-g", url])
raw_url = raw_url |> String.trim()
```

## Audio Timeout
Upon invoking `Nostrum.Voice.play/4`, the player process has a large configurable initial window
(`20_000` milliseconds by default) that it must generate audio within before timing out. This is done to allow
ample time for slow networks to download large audio/video files. This configurable timeout only applies to when
`play` is initially invoked; once audio has begun transmitting, the timeout drops to `500` milliseconds.
Because the `ffmpeg` process doesn't close when its input device is `stdin`, which is the case
when `type` is set to `:pipe`, `:ytdl`, or `:stream` the timeout is necessary to promptly detect end of input.
If the audio process times out within the initial window, the `Nostrum.Struct.Event.SpeakingUpdate`
that is generated will have its `timed_out` field set to `true`. It will be `false` in all other cases.
If your use case does not include large, slow downloads and you wish to more quickly be notified
of timeouts or errors, you may consider setting `audio_timeout` to a lower value.
However, `youtube-dl` typically takes at least 2.5 seconds to begin outputting audio data,
even on a fast connection.
If your use case involves playing large files at a timestamp several hours in like this,
`play(guild_id, url, :ytdl, start_time: "2:37:56")`, you may consider setting the timeout to a higher value,
as downloading a large youtube video and having `ffmpeg` seek through several hours
of audio may take 15-20 seconds, even with a fast network connection.

## Audio Frames Per Burst
The value `:audio_frames_per_burst` represents the number of consecutive packets to send before resting.
When using `Nostrum.Voice.play/4` to play audio, Nostrum collects a number of opus frames from the 
audio input source before sending them all to Discord as a "burst" of ordered frames. 
This is done to reduce the overhead of process-sleeping and setup.
For reference, a single opus frame is 20 milliseconds of audio (at least for the format that Discord uses).
By default, the `:audio_frames_per_burst` is set to `10`, equivalent to 200 milliseconds of audio.

Under normal circumstances, there's no reason to change this value. However, if you attempt to play a very short
piece of audio that's less than `10` frames (200ms) in length, it will time out (after the configured 
`:audio_timeout` duration has passed) as it waits to collect `10` frames to send. For those cases, configure the
value to *at most* the minimum frame length of the audio you intend to play, or simply `1`. Setting the value to `1`
means that each opus frame from your audio source will be taken individually and be sent in its own "burst" with the 
player process sleeping between each; you likely won't notice a difference in audio playback quality compared to the 
default value of `10` other than that your sub-200ms audio files will play as expected.

## Voice Events
There are a few voice related events that bots can consume with a `Nostrum.Consumer` process:
  - `t:Nostrum.Consumer.voice_state_update/0`
  - `t:Nostrum.Consumer.voice_server_update/0`
  - `t:Nostrum.Consumer.voice_speaking_update/0`
  - `t:Nostrum.Consumer.voice_ready/0`
  - `t:Nostrum.Consumer.voice_incoming_packet/0`

Both `t:Nostrum.Consumer.voice_state_update/0` and `t:Nostrum.Consumer.voice_server_update/0` 
are sent by the shard gateway session when a bot joins a voice channel. The receipt of both of 
these events is required for a voice gateway session to begin, and it happens automatically when 
joining a channel. The `t:Nostrum.Consumer.voice_state_update/0` event is also sent every time 
any user joins or leaves a voice channel, and `t:Nostrum.Struct.Guild.voice_states/0` is 
automatically updated within the guild cache to reflect current state of voice channels.

A use case for listening to both `t:Nostrum.Consumer.voice_state_update/0` and 
`t:Nostrum.Consumer.voice_server_update/0` events would be to outsource voice connections to 
an application outside of Nostrum. This can be done by setting the config option 
`:voice_auto_connect` to `false` and taking the session and token information 
from both of the events and passing them to your external voice app. 
Outside of this niche use case, another use case for listening solely to the
`t:Nostrum.Consumer.voice_state_update/0` event would be to detect when users join or leave 
voice channels.

The `t:Nostrum.Consumer.voice_speaking_update/0` event is generated by Nostrum for convenience. It is 
sent every time the bot starts or stops speaking/sending audio. A use case for this event is if 
you have a queue of URLs to play, listening to the `t:Nostrum.Consumer.voice_speaking_update/0` 
will let the bot know when the current URL has finished playing and that it should begin playing 
the next one in the queue. The alternative approach for this use case that is not event-driven 
is to periodically call `Nostrum.Voice.playing?/1` and wait for it to return `false` as the 
trigger to play the next URL. Note that the third element in the event is of type
`t:Nostrum.Struct.VoiceWSState.t/0` and not `t:Nostrum.Struct.WSState.t/0`.

The `t:Nostrum.Consumer.voice_ready/0` event is generated by Nostrum for convenience. It is sent
when the bot is ready to begin sending audio data upon joining a voice channel. From the moment
the bot joins a voice channel, Nostrum handles the multi-step handshaking process that is 
required before any audio packets can be sent or received. It is a common use case for bots to
immediately begin playing audio upon joining a voice channel. Calling `Nostrum.Voice.play/4`
directly after calling `Nostrum.Voice.join_channel/4` will always return an error as several
network actions must take place before playing audio is possible. Listening for the
`t:Nostrum.Consumer.voice_ready/0` event can be used by the bot to begin playing audio as soon 
as it is able to. The alternative approach for this use case that is not event-driven is to 
periodically call `Nostrum.Voice.ready?/1` and wait for it to return `true` as the trigger to
begin playing. Another common approach is to define a `try_play` function as follows:
```elixir
def try_play(guild_id, url, type, opts \\ []) do
  case Nostrum.Voice.play(guild_id, url, type, opts) do
    {:error, _msg} ->
      Process.sleep(100)
      try_play(guild_id, url, type, opts)

    _ ->
      :ok
  end
end
```
Note that the third element in the event is of type `t:Nostrum.Struct.VoiceWSState.t/0` and 
not `t:Nostrum.Struct.WSState.t/0`.

The `t:Nostrum.Consumer.voice_incoming_packet/0` event is generated by Nostrum. None will be generated
by default. You must first be connected to a voice channel, call the `Nostrum.Voice.start_listen_async/1`
function, then have another user in the same voice channel speak. If these conditions are met, an event
will be received for each RTP packet the bot receives; 50 packets per 1 second for each user that is
actively speaking. These events are only useful if you intend to listen to incoming audio and are disabled
by default. An alternative approach to listening to incoming audio that is not event driven is to call
`Nostrum.Voice.listen/3`. This function blocks until the specified number of RTP packets is received.
`Nostrum.Voice.listen/3` has the additional features of removing duplicate RTP packets within the set of
packets returned per invocation and the option to return the raw RTP packet. In practice these features
likely won't be missed when consuming incoming voice packets asynchronously.
Note that the third element in the event is of type
`t:Nostrum.Struct.VoiceWSState.t/0` and not `t:Nostrum.Struct.WSState.t/0`.
