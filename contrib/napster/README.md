Playdar napster resolver
========================

Create a file that looks like this:

	$ cat playdar-core/etc/napster.conf
	[napsdar]
	apikey=napsterApiKey

If you have a napster account, you can add 
	username=foo
	password=bar

If you don't add ausername and password then you will only be
able to play 30 second streams.

To install, edit etc/playdar.conf and add a section like this:

	{scripts,[
	"contrib/napsdar/napster_resolver.py"
	]}.

If you run OSX or Linux, make sure /usr/bin/python exists.
Suggestions welcome for setup on Windows.
