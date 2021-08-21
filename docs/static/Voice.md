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
Nostrum also has support for `streamlink`, yet another powerful command line utility for downloading live streams from online video streaming services.
By default Nostrum will look for the executable `streamlink` in the system path. 
If the executable is elsewhere, the path may be configured via `config :nostrum, :streamlink, "/path/to/streamlink"`.
When `Nostrum.Voice.play/4` is called with `:stream` for the `type` parameter, `streamlink` 
will be attempt to download the live stream content and pipe it to `ffmpeg`.
Using `streamlink` with Nostrum depends on `youtube-dl` to get the underlying
stream URL from the user-friendly URL that's given as input.

## Audio Timeout
Upon invoking `Nostrum.Voice.play/4`, the player process has a large configurable initial window
(`20_000` milliseconds by default) that it must generate audio within before timing out. This is done to allow
ample time for slow networks to download large audio/video files. This configurable timeout only applies to when
`play` is initially invoked; once audio has begun transmitting, the timeout drops to `500` milliseconds.
Because the `ffmpeg` process doesn't close when its input device is `stdin`, which is the case
with `type` is set to `:pipe` or `:ytdl`, the timeout is necessary to promtly detect end of input.
If the audio process times out with the initial window, the `Nostrum.Struct.Event.SpeakingUpdate`
that is generated will have its `timed_out` field set to `true`. It will be `false` in all other cases.
If your use case does not include large, slow downloads and you wish to more quickly be notified
of timeouts or errors, you may consider setting `audio_timeout` to a lower value.
However, `youtube-dl` typically takes at least 2.5 seconds to begin outputting audio data,
even on a fast connection.
If your use case involves playing large files at a timestamp several hours in like this,
`play(guild_id, url, :ytdl, start_time: "2:37:56")`, you may consider setting the timeout to a higher value, as downloading a large youtube video and having `ffmpeg` seek through several hours
of audio may take 15-20 seconds, even with a fast network connection.