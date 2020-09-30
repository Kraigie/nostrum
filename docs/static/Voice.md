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
The function `Nostrum.Voice.play/3` allows sound to played via files or via raw
data that gets piped to `stdin` of the `ffmpeg` instance.