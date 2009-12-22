#!/usr/bin/python
# An audiofarm.org resolver by Kit Sunde <kitsunde@gmail.com>
#
# In your etc/playdar.conf scripts section, add
# "contrib/audiofarm/audiofarm_respolver.py" to see this in action.
#
# This uses the audiofarm API as it were on 2009-12-21 
# API: http://www.audiofarm.org/home/developer
# The API currently does not give a direct download link and playdar had
# issues resolving the direct-download link on the download page which
# all seem to have permanent redirects and require a (certain?) User-Agent, so
# the resolver follows the redirects and instead gives the real link to playdar.
#
# Possible areas for improvement:
# * If search return a huge list of valid tracks there's a risk of timing out
#   when we don't have too. Maybe the resolver should figure out the details for
#   them in parallell.
import playdar_resolver
import urllib2, urllib, sys, re
import xml.etree.ElementTree as ET


class AudioFarmResolver( playdar_resolver.PlaydarResolver ):
	def resolver_settings( self ):
		"""My settings"""
		return {
			'targettime':5000,
			'name':"Audiofarm Resolver"
		}

	def searchFor( self, query ):
		"""Uses audiofarms API to perform a search and returns an ElementTree
		with the results"""
		search_url = "http://www.audiofarm.org/%s/api/audiofiles/search"
		opener = self.getOpener()
		artistTrack = (query['artist'] + ' ' + query['track']).strip()
		look_for = urllib.quote_plus( artistTrack )
		fp = opener.open( search_url % ( look_for ) )
		return ET.parse( fp )

	def getOpener( self ):
		"""audiofarm requires us to have a User-agent or it won't return
		anything"""
		opener = urllib2.build_opener()
		opener.addheaders = [ ( 'User-agent', "Mozilla/5.0" ) ]
		return opener		

	def getSoundFile( self, publisher, audio_id ):
		"""Finds the real download URL and returns it. Returns False if we can't
		get a direct link which happens on some licenses that are preview-only
		through their embeded player on the download page."""
		opener = self.getOpener()
		download_url = "http://www.audiofarm.org/%s/audiofile/%s.mp3"
		try:
			return opener.open( download_url % ( publisher, audio_id ) )
		except urllib2.URLError:
			return False

	def getScore( self, info, query ):
		"""Calculate a weight to the results, so we can distinguish between
		good and bad results."""
		bad_words = ["podcast", "live"]
		score = 1.0
		for bad_word in bad_words:
			if( bad_word in info['track'].lower()
				and not bad_word in query['track'] ):
				score -= 0.1
		return score

	def splitArtistFromTrack( self, info, query ):
		"""Returns a dict with the artist and track if possible"""
		reply = { 'track' : info['track'] }
		if (not query['track'].count(' - ') > 0
			and info['track'].count(' - ') == 1):
			split_track = info['track'].split(' - ')
			reply['artist'] = split_track[0]
			reply['track'] = split_track[1]

		return reply

	def getArtistAndTrack( self, info, query, download_page ):
		"""Returns a dict with the track and artist (if we can find the artist
		name). Sometimes the artist is in the name and sometimes on the download
		page."""
		results = self.splitArtistFromTrack( info, query )
		if 'artist' in results: return results
		results.update( self.getArtistFromDlPage( download_page ) )
		return results

	def getArtistFromDlPage( self, download_page ):
		"""Returns a dict with he artist if we can find it on the download
		page"""
		opener = self.getOpener()
		try:
			response = opener.open( download_page ).read()
		except urllib2.URLError:
			return None
		try:
			return {
				'artist' : re.search( "/labels/([\w\s]+)", response ).group( 1 )
			}
		except AttributeError:
			return None

	def results( self, query ):
		"""Resolves audiofarm. Tries to favor their API for results but it's
		lacking for our purposes so we (sometimes) have to scrape the track
		page for the artist name and we connect to the music file tog get the
		real url."""
		searchTree = self.searchFor( query )

		results = []

		ns = {"audiofile" : "http://www.audiofarm.org/home/rdf_specs#",
			"rdf" : "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}

		for result in searchTree.findall( "{%(audiofile)s}track" % ns ):
			info = {}
			publisher = result.findtext( "{%(audiofile)s}publisher" % ns )
			info['track'] = result.findtext( "{%(audiofile)s}name" % ns )

			# Skip tracks that doesn't have the whole track search name
			if query['track'].lower() not in info['track'].lower(): continue

			audio_id = result.findtext( "{%(audiofile)s}id" % ns )
			response = self.getSoundFile( publisher, audio_id )
			if response is False: continue
			info['url'] = response.url

			d_element = result.find( "{%(audiofile)s}download_link" % ns )
			download_link = d_element.attrib.get( "{%(rdf)s}resource" % ns )
			info.update( self.getArtistAndTrack( info, query, download_link ) )

			# The API seem to only give content-type and byte sometimes so we
			# have to get it from the response instead
			info['mimetype'] = response.info().getheader("Content-Type")
			info['size'] = response.info().getheader("Content-Length")
			info['score'] = self.getScore( info, query )
			info['source'] = 'audiofarm.org'
			results.append( info )
		return results

if __name__ == "__main__":
	try:
		AudioFarmResolver.start_static()
	except:
		traceback.print_exc(file=sys.stderr)
