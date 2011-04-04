# -*- coding: utf8 -*-
# This class speaks to the Ampache XML API
#
# author:  Remo Giermann <mo@liberejo.de>
# created: 2010/03/31
# 

import time, urllib
from hashlib import sha256
from xml.etree.ElementTree import ElementTree

def passphrase(password):
	"""
	Generates the PASSPRHASE needed for a handshake.
	"""
	now = int(time.time())
	key = sha256(password).hexdigest()
	phrase = sha256(str(now)+key).hexdigest()

	return (now, phrase)


class AmpacheArtist(object):
	"""
	Simple container class corresponding to
	Ampache's Artist XML.
	"""
	def __init__(self, element):
		self.id = element.attrib['id']
		self.tags = element.findall('tag')
		self.name = element.find('name').text

		self.albums = element.find('albums').text
		self.songs = element.find('songs').text

		self.preciserating = element.find('preciserating').text
		self.rating = element.find('rating').text

class AmpacheAlbum(object):
	"""
	Simple container class corresponding to
	Ampache's Album XML.
	"""
	def __init__(self, element):
		self.id = element.attrib['id']
		self.tags = element.findall('tag')
		self.name = element.find('name').text
		artist = element.find('artist')
		self.artist = {'id': artist.attrib['id'], 'name': artist.text}

		self.year = element.find('year').text
		self.tracks = element.find('tracks').text
		self.disk = element.find('disk').text
		self.art = element.find('art').text
		
		self.preciserating = element.find('preciserating').text
		self.rating = element.find('rating').text

class AmpacheSong(object):
	"""
	Simple container class corresponding to
	Ampache's Song XML.
	"""
	def __init__(self, element):
		self.id = element.attrib['id']
		self.tags = element.findall('tag')
		self.title = element.find('title').text
		artist = element.find('artist')
		self.artist = {'id': artist.attrib['id'], 'name': artist.text}
		album = element.find('album')
		self.album = {'id': album.attrib['id'], 'name': album.text}

		self.track = element.find('track').text
		self.time = element.find('time').text
		self.url = element.find('url').text
		self.size = element.find('size').text
		self.art = element.find('art').text
		
		self.preciserating = element.find('preciserating').text
		self.rating = element.find('rating').text


class Ampache(object):
	"""
	This class speaks to an Ampache server through its
	XML API. Only a few methods are implemented, but 
	every method of the API could be added quickly.

	See http://ampache.org/wiki/dev:xmlapi for info.
	"""

	def __init__(self, url):
		self.auth = ''
		self.serverurl = url + u"/server/xml.server.php?"
	
	
	def __query(self, **kwargs):
		for k,v in kwargs.items():
			if type(v) is unicode:
				kwargs[k] = v.encode('utf8')
		query = urllib.urlencode(kwargs)
		
		try:
			fd = urllib.urlopen(self.serverurl+query)
		except IOError:
			self.auth = ''
			return None
		else:
			tree = ElementTree()
			tree.parse(fd)
			return tree


	def __action(self, action, **kwargs):
		if not self.auth:
			return False

		return self.__query(action=action, auth=self.auth, **kwargs)


	def handshake(self, username, password):

		timestamp, phrase = passphrase(password)
		tree = self.__query(action='handshake', auth=phrase, timestamp=timestamp, version=350001, user=username)
		
		if tree is None:
			auth = None
		else:
			auth = tree.find('auth')
		
		
		if auth is None:
			self.auth = ''
			return False
		else: 
			self.auth = auth.text
			return True


	# API method 'artist'
	def artists(self, filter, **kwargs):
		tree = self.__action('artists', filter=filter, **kwargs)
		artists = tree.findall('artist')
		return [AmpacheArtist(a) for a in artists]

	# API method 'artist_songs'
	def artist_songs(self, artist_uid):
		tree = self.__action('artist_songs', filter=artist_uid)
		songs = tree.findall('song')
		return [AmpacheSong(a) for a in songs]

	# API method 'artist_albums'
	def artist_albums(self, artist_uid):
		tree = self.__action('artist_albums', filter=artist_uid)
		albums = tree.findall('album')
		return [AmpacheAlbum(a) for a in albums]
	
	# API method 'albums'
	def albums(self, filter, **kwargs):
		tree = self.__action('albums', filter=filter, **kwargs)
		albums = tree.findall('albums')
		return [AmpacheAlbums(a) for a in albums]
	
	# API method 'album_songs'	
	def album_songs(self, album_uid):
		tree = self.__action('album_songs', filter=album_uid)
		songs = tree.findall('song')
		return [AmpacheSong(a) for a in songs]
	
	# API method 'songs'
	def songs(self, filter, **kwargs):
		tree = self.__action('songs', filter=filter, **kwargs)
		songs = tree.findall('song')
		return [AmpacheSong(a) for a in songs]
	
	# API method 'search_songs'
	def search_songs(self, filter, **kwargs):
		tree = self.__action('search_songs', filter=filter, **kwargs)
		songs = tree.findall('song')
		return [AmpacheSong(a) for a in songs]
