#!/usr/bin/python

# Playdar Napster resolver
# Copyright 2009, 2010 Alastair Porter
# Released under the MIT License

import playdar_resolver
import napster
import sys
import traceback

class NapsterResolver(playdar_resolver.PlaydarResolver):
	def __init__(self):
		napster.connect()
	def resolver_settings(self):
		return {'name':"Napster Resolver"}

	def results(self, query):
		data = napster.getStreamData(query['artist'], query['album'], query['track'])
		if data is None:
			return []

		res = []
		for track in data:
			res.append({
				'artist': track["artist"],
				'track' : track["track"],
				'album' : track["album"],
				'source' : "Napster",
				'url' : track["url"],
				'duration' : track["duration"],
				'score' : 1.00
					})
		return res	
		
if __name__ == "__main__":
	try:
		NapsterResolver.start_static()
	except:
		traceback.print_exc(file=sys.stderr)
