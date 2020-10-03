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
The function `Nostrum.Voice.play/3` allows sound to played via files, local or 
remote, or via raw data that gets piped to `stdin` of the `ffmpeg` process.
When playing from a url, the url can be a name of a file on the filesystem or a url
of file on a remote server - [ffmpeg supports a ton of protocols](https://www.ffmpeg.org/ffmpeg-protocols.html),
of which the most populat are probably `http` or simply reading a file from the filesystem.

## youtube-dl
With only `ffmpeg` installed, Nostrum supports playing audio/video files or raw, piped
data as discussed in the section above. Nostrum also has support for `youtube-dl`, another
powerful command line utility for downloading audio/video from online video services.
Although the name implies support for Youtube, `youtube-dl` supports downloading from
[an immense list of sites](https://github.com/ytdl-org/youtube-dl/blob/master/docs/supportedsites.md).
Be default Nostrum will look for the executable `youtube-dl` in the system path. If the
executable is elsewhere, the path may be configured via `config :nostrum, :youtubedl, "/path/to/youtube-dl"`. When `Nostrum.Voice.play/3` is called with `:ytdl` for the `type` parameter,
`youtube-dl` will be running with options `-f bestaudio -o -`, which will attempt to download
the audio at the given url and pipe it to `ffmpeg`.
