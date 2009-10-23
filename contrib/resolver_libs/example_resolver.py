#!/usr/bin/python
# In your etc/playdar.conf scripts section, add "contrib/resolver_libs/example_respolver.py"
# to see this in action.
import playdar_resolver
from playdar_resolver import soundex

class ExampleResolver(playdar_resolver.PlaydarResolver):
	def resolver_settings(self):
		"""My settings"""
		return {'name':"Example Python Resolver"}

	def results(self, query):
		if query['artist'].lower() != 'mokele':
			return []
		if soundex(query['track']) != soundex('hiding in your insides'):
			return []

		return [{
			'artist': "Mokele",
      'track' : "Hiding In Your Insides (python)",
      'album' : "You Yourself are Me Myself and I am in Love",
      'source' : "Mokele.co.uk",
      'size' : 4971780,
      'bitrate' : 160,
      'duration' : 248,
      # NB this url should be url encoded properly:
      'url' : "http://play.mokele.co.uk/music/Hiding%20In%20Your%20Insides.mp3",
      'score' : 1.00
		}]
		
if __name__ == "__main__":
	try:
		ExampleResolver.start_static()
	except:
		traceback.print_exc(file=sys.stderr)