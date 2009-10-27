#!/usr/bin/python
"""
Base class for Python resolvers.
"""
import sys, traceback
from httplib import HTTP
# easy_install simplejson
import simplejson as json
from urlparse import urlparse
import urllib, urllib2
import socket
from xml.dom import minidom
from struct import unpack, pack

def soundex(name, len=4):
    """ soundex module conforming to Knuth's algorithm
        implementation 2000-12-24 by Gregory Jorgensen
        public domain
    """

    # digits holds the soundex values for the alphabet
    digits = '01230120022455012623010202'
    sndx = ''
    fc = ''

    # translate alpha chars in name to soundex digits
    for c in name.upper():
        if c.isalpha():
            if not fc: fc = c   # remember first letter
            d = digits[ord(c)-ord('A')]
            # duplicate consecutive soundex digits are skipped
            if not sndx or (d != sndx[-1]):
                sndx += d

    # replace first digit with first alpha character
    sndx = fc + sndx[1:]

    # remove all 0s from the soundex code
    sndx = sndx.replace('0','')

    # return soundex code padded to len characters
    return (sndx + (len * '0'))[:len]

def print_result(o):
	s = json.dumps(o)
	debug("Writing: " + s)
	sys.stdout.write(pack('!L', len(s)))
	sys.stdout.write(s)
	sys.stdout.flush()

def debug(message):
	pass
	#sys.stderr.write(str(message) + "\n")
	#sys.stderr.flush()

class PlaydarResolver:
	def results(self, query):
		"""Method to return results, override this."""
		return []
	
	def resolver_settings(self):
		"""Method to set custom settings, override this."""
		return {}
		
	def settings(self):
		"""Generic settings to be overridden by resolver_settings."""
		debug("settings")
		settings =  {
			'_msgtype':'settings', 
			'name':'Generic Python Resolver', 
			'targettime':5000, 
			'weight':50
		}
		settings.update(self.resolver_settings())
		debug(settings)
		return settings
		
	def resolve(self, query):
		"""Resolve a query, passing it to the subclass's results function."""
		return {'_msgtype':'results', 'qid': query['qid'], 'results':self.results(query)}
	
	@classmethod
	def start_static(cls):
		"""Start from class"""
		return cls().start()
		
	def start(self):
		"""Starts the resolver"""
		print_result(self.settings())
		
		while 1:
			length = sys.stdin.read(4)

			if not length:
					break;

			length = unpack('!L', length)[0]
			if not length:
					break
			# something probably went wrong, most likely we're out of sync and are 
			# reading the 4 bytes length header in the middle of a json string. We can't
			# recover. Bail.
			if length > 4096 or length < 0:
					break
			if length > 0:
					msg = sys.stdin.read(length)
					try:
							request = json.loads(msg)
							# print request
							results = self.resolve(request)
							print_result(results)
					except:
							traceback.print_exc(file=sys.stderr)
							# safe to continue, skipping this msg, because at least
							# we consumed enough input so next iteration hits size header.
							pass