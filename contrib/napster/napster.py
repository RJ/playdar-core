#!/usr/bin/python

# Playdar Napster resolver
# Copyright 2009, 2010 Alastair Porter
# Released under the MIT License

import urllib
import urllib2
import urlparse
import xml.etree.ElementTree
import re
from htmlentitydefs import name2codepoint
import httplib
import os
import ConfigParser
import sys
import time
import pickle
import playdar_resolver

session_created=False
session_key = ""
session_expiry = 0

# XXX: For the hackday this can be anything - should be a MAC or something
DEVICE_ID="hack"

memocache = {}
def memoify(func):
	def memoify(*args,**kwargs):
		now = time.time()
		if func.__name__ not in memocache:
			memocache[func.__name__]={}
		key=pickle.dumps((args,kwargs))
		if key in memocache[func.__name__]:
			lastrun, data = memocache[func.__name__][key]
			if lastrun + 50 * 60 < now:
				# delete so that playurls are still valid
				del memocache[func.__name__][key]

		if key not in memocache[func.__name__]:
			memocache[func.__name__][key]=(now, func(*args,**kwargs))

		return memocache[func.__name__][key][1]
	return memoify

def connect():
	global memocache
	memocache = {}

	config=ConfigParser.RawConfigParser()
	config.add_section("napster")
	config.set("napster", "username", "")
	config.set("napster", "password", "")
	config.set("napster", "apikey", "")
	config.read(os.path.join(os.getcwd(), "etc", "napster.conf"))
	user = config.get("napster", "username")
	passw = config.get("napster", "password")
	apiKey = config.get("napster", "apikey")
	if apiKey == "":
		raise Exception("Need an API key (napster.apikey)")

	if user != "" and passw != "":
		_login(apiKey, user, passw)
	else:
		_createSession(apiKey)

def _createSession(apiKey):
	global session_created, session_key, session_expiry
	res = _do_napster_query("security/createSession", apiKey=apiKey, deviceId=DEVICE_ID)
	session_key = res['sessionKey'][0]
	session_created = True
	session_expiry = time.time() + int(res['minutesUntilExpiry'][0]) * 60

def _login(apiKey, user, passw):
	global session_created, session_key, session_expiry
	res = _do_napster_query("security/login", apiKey=apiKey, deviceId=DEVICE_ID, username=user, password=passw)
	session_key = res['sessionKey'][0]
	session_created = True
	session_expiry = time.time() + int(res['minutesUntilExpiry'][0]) * 60

def htmlentitydecode(s):
    os= re.sub('&(%s);' % '|'.join(name2codepoint), 
            lambda m: unichr(name2codepoint[m.group(1)]), s)
    return os

def _cleanname(x):
	if x is None:
		return ''
	return htmlentitydecode(x)

def _etree_to_dict(etree):
	result={}
	for i in etree:
		if i.tag not in result:
			result[i.tag]=[]
		if len(i):
			result[i.tag].append(_etree_to_dict(i))
		else:
			result[i.tag].append(_cleanname(i.text))
	return result

def _parse_tree(f):
	tree = xml.etree.ElementTree.ElementTree(file=f)
	return _etree_to_dict(tree.getroot())

def _do_napster_query(method, **kwargs):
	""" You probably don't want to use this to do a query -
	    use _do_checked_query instead"""
	args = {}
	for k,v in kwargs.items():
		args[k] = v.encode("utf8")

	url=urlparse.urlunparse(('https',
		'api.napster.com:8443',
		'/rest/v4/%s' % method,
		'',
		urllib.urlencode(args),
		''))

	#print >> sys.stderr, "opening url",url
	f = urllib2.Request(url)
	try:
		f = urllib2.urlopen(f)
	except Exception, e:
		print >> sys.stderr, e.msg
		print >> sys.stderr, e.fp.read()
		raise

	return _parse_tree(f)

@memoify
def _do_checked_query(method, **kwargs):
	if not session_created:
		raise Exception("Login or create a session first (use connect())")

	if time.time() > session_expiry:
		# re-login if session has expired
		connect()

	kwargs["sessionKey"] = session_key

	return _do_napster_query(method, **kwargs)

def searchArtists(name):
	return _do_checked_query("search/artists", searchTerm=name)

def searchTracks(title):
	return _do_checked_query("search/tracks", searchTerm=title)

def searchAlbums(name):
	return _do_checked_query("search/albums", searchTerm=name)

def page(query, **kwargs):
	start = 0
	skipNum = 50
	kwargs["startPosition"] = str(start)
	kwargs["maxResults"] = str(skipNum)
	res = _do_checked_query(query, **kwargs)
	numRes = res["numberOfResults"][0]
	total = int(numRes)
	while total > 0:
		# This does the query again, but it's cached
		kwargs["startPosition"] = str(start)
		kwargs["maxResults"] = str(skipNum)
		yield _do_checked_query(query, **kwargs)
		start += skipNum
		total -= 50

def artistTrackSearch(artist, track):
	ret = []
	artists = searchArtists(artist)
	artistlist = []
	artists = artists.get('artist', [])
	# If we have 1 artist and a 'simple' track name (1/2 words), 
	# use the artist track listing instead, so we don't have to 
	# do a full track search
	if len(artists) == 1 and len(track.split(" ")) < 3:
		artistUrl = "artists/%d/tracks" % int(artists[0]['id'][0])
		for trackPage in page(artistUrl):
			for tr in trackPage.get('track', []):
				if tr['trackName'][0].lower() == track.lower():
					ret.append(_make_track_result(tr))
	else:
		for artist in artists:
			artistlist.append(artist['restArtistURL'][0])
		for track in page("search/tracks", searchTerm=track):
			for tr in track.get('track', []):
				artistUrl = tr['artistResourceURL'][0]
				if artistUrl in artistlist:
					ret.append(_make_track_result(tr))
	return ret

def getStreamData(artistName, albumName, title):
	artists = searchArtists(artistName)
	artistlist = []
	for artist in artists.get('artist', []):
		artistlist.append(artist['restArtistURL'][0])
	if len(artistlist) == 0:
		return []

	albumlist = []
	if len(albumName) > 0:
		albums = searchAlbums(albumName)
		for album in albums.get('album', []):
			if album['artistResourceURL'][0] in artistlist:
				albumlist.append(album)
		if len(albumlist) == 0:
			# no album, try a track search instead
			# (may be on a best-of?)
			return artistTrackSearch(artistName, title)
		elif len(albumlist) == 1:
			# intersection of album/artist search is 1 album
			albumurl = "albums/%s" % os.path.basename(albumlist[0]['albumResourceURL'][0])
			albumdata = _do_checked_query(albumurl)
			for tr in albumdata['tracks']:
				if tr['trackName'][0].lower() == title.lower():
					return [_make_track_result(tr)]
			# If no matches, try a soundex - fixes the Foxy Lady/Foxey Lady problem at least!
			for tr in albumdata['tracks']:
				if playdar_resolver.soundex(tr['trackName'][0]) == playdar_resolver.soundex(title):
					return [_make_track_result(tr)]
			# no match, do a track search
			return artistTrackSearch(artistName, title)
		else:
			# more than 1 match.  track search?
			return artistTrackSearch(artistName, title)

	else: # No album, just do an artist/track search
		return artistTrackSearch(artistName, title)


def _make_track_result(track):
	url = track['playTrackURL'][0]
	url += "?sessionKey="+session_key
	# Napster currently returns a http:// url but the https port, so change it.
	url = url.replace(":8443", ":8080")

	duration = track['duration'][0]
	durparts = duration.split(":")
	if len(durparts) == 2:
		length = int(durparts[0]) * 60 + int(durparts[1])
	else:
		length = -1

	return {
		"url": url,
		"artist": track['artistName'][0],
		"track": track['trackName'][0],
		"album": track['albumName'][0],
		"duration": length }

def test():
	import time
	start = time.time()
#	print "starting at", start
	connect()
	m = time.time()
#	print "Logged in after",m-start,"secs"
	print getStreamData(sys.argv[1], sys.argv[2], sys.argv[3])
#	print "got results after",time.time()-m,"secs"

if __name__ == "__main__":
	test()
