#!/usr/bin/env python
# -*- coding: utf8 -*-
#
# Ampache resolver for playdar
# using Ampache's Web API
#
# author: Remo Giermann <mo@liberejo.de>
# created: 2011/04/03
#

import playdar_resolver
import ampache
import os, sys
from ConfigParser import ConfigParser
from playdar_resolver import soundex


configuration = os.path.join(os.path.dirname(sys.argv[0]), "ampache-resolver.cfg")


class AmpacheResolver(playdar_resolver.PlaydarResolver):
	
	def __init__(self):
		
		self.ampaches = []
		
		config = ConfigParser()
		if not config.read(configuration):
			return
		else:
			for section in config.sections():
				if config.options(section) == ['url', 'password', 'user']:
					url = config.get(section, 'url')
					un = config.get(section, 'user')
					pw = config.get(section, 'password')

					amp = ampache.Ampache(url)
					if amp.handshake(un, pw) is True:
						self.ampaches.append((amp, "Ampache (%s)" % section))
				
	
	def resolver_settings(self):
		"""My settings"""
		return {'name':"Ampache (API) Resolver", 'targettime':5000, 'weight':60}


	def results(self, query):
		results = []
		
		for amp, name in self.ampaches:
			
			songs = amp.search_songs(u'{artist} {track}'.format(**query))
			
			# first item in search results is best match, last item is worst,
			# so scoring is simple:
			score = 1.0
			for s in songs:

				if s.artist['name'].lower() == query['artist'].lower() \
				  and score > 0.5: 
					
					results.append( 
						{'artist': s.artist['name'], 
						'track': s.title, 
						'album': s.album['name'],
						'duration': int(s.time),
						'url': s.url,
						'score': score,
						'extension': s.url[-3:],
						'art': s.art,
						#'size': int(s.size),
						'source': name
					})
					# decrement score for next match
					score -= .1
					
				else: 
					break
				

		return results
	
	
	
if __name__ == "__main__":
	#q = {'qid':'test', 'artist': 'iron maiden', 'track': 'aces high'}
	#r = AmpacheResolver()
	#playdar_resolver.print_result(r.resolve(q))
	try:
		AmpacheResolver.start_static()
	except:
		traceback.print_exc(file=sys.stderr)
