#!/bin/bash
read method url version
method="${method%$CR}"
url="${url%$CR}"
version="${version%$CR}"
echo -ne "HTTP/1.0 200 OK\r\nContent-type: audio/mpeg\r\n\r\n"
lynx --source "`echo "$url" | sed 's/^\///g'`" | ( ffmpeg -i - -acodec libmp3lame -ac 2 -ab 128000 -f mp3 - 2>/dev/null)
