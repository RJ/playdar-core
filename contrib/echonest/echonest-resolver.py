#!/usr/bin/python
# -*- coding: utf-8 -*-
# Created by Paul Lamere<Paul.Lamere@gmail.com> twitter.com/plamere
# Based on the seeqpod resolver by:
# Max Howell <max@methylblue.com> twitter.com/mxcl
# Licensed the same as Playdar
#
# Uses pyechonest available at http://code.google.com/p/pyechonest/
# http://bitbucket.org/rg3/youtube-dl/wiki/Home

########################################################################
import sys
from httplib import HTTP
import simplejson as json
from urlparse import urlparse
import urllib, urllib2
import socket
from xml.dom import minidom
from struct import unpack, pack

from  pyechonest import artist as artist_api
from  pyechonest import config as echo_nest_config

######################################
# Echo Nest config
# put your echo nest API key here
# echo_nest_config.ECHO_NEST_API_KEY = 'your api key'
#
echo_nest_config.TRACE_API_CALLS = False
echo_nest_config.OBEY_RATE_LIMIT = False

##########################################
# Echo nest resolver
#

# given an 
def get_tracks(artist_name, track_name):
    try:
        print >>sys.stderr, 'EN searching for', artist_name, track_name, '\r'
        tracks = []
        artists = artist_api.search_artists(artist_name)
        for artist in artists:
            print >>sys.stderr, 'EN artist', artist.name, '\r'
            for audio in artist.audio(1000):
                if 'title' in audio and fuzzy_match(track_name, audio['title']):
                    print >>sys.stderr, 'EN audio', audio['title'], '\r'
                    if is_live(audio['url']):
                        track = {}
                        track['artist'] = audio['artist']
                        track['track'] = audio['title']
                        track['album'] = audio['release']
                        track['url'] = audio['url']
                        track['duration'] = audio['length']
                        # Is the best source 'echonest' or the blog
                        # where the track was found?
                        track['source'] = 'echo nest'

                        # BUG: need a better score
                        track['score'] = 0.99
                        tracks.append(track)
                        # if we found one we are done:
                        print >>sys.stderr, 'EN found track', track['url'] , '\r'
                        return tracks;
    except:
        print >>sys.stderr, 'EN had an error', '\r'
    print >>sys.stderr, 'EN found nothing', '\r'
    return tracks


def is_live(url):

    try:
        socket.setdefaulttimeout(5)
        f = urllib.urlopen(url)
        is_audio = f.info().gettype().find("audio") >= 0
        f.close()
        return is_audio
    except IOError:
        return False

def fuzzy_match(query, match):
    # we need a better fuzzy match than this
    query = query.lower()
    match = match.lower()
    return match.find(query) >= 0
   
###################################################################### functions
def print_json(o):
    s = json.dumps(o)
    sys.stdout.write(pack('!L', len(s)))
    sys.stdout.write(s)
    sys.stdout.flush()

def percent_encode(url):
    # Ha! Yeah, needs work
    return url.replace(' ', '%20')

def resolve(artist, track):
    return get_tracks(artist, track)


def test():
    print 'is live', is_live('http://apt104.com/Apr%2009/paris/04-Heartbreaker.mp3')
    for t in get_tracks("weezer", "buddy holly"):
        print t

#test()
####################################################################### settings
settings = dict()
settings["_msgtype"] = "settings"
settings["name"] = "Echo Nest Resolver"
settings["targettime"] = 5000 # millseconds
settings["weight"] = 50 # echo nest results aren't as good as friend's results
print_json( settings )

###################################################################### main loop
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
            tracks = resolve(request['artist'], request['track'])
            if len(tracks) > 0:
                response = { 'qid':request['qid'], 'results':tracks, '_msgtype':'results' }
                print_json(response)
        except:
            # safe to continue, skipping this msg, because at least
            # we consumed enough input so next iteration hits size header.
            pass
