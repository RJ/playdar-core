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

from amieapi import AmieStreet

 
##########################################
# Echo nest resolver
#

# given an
def get_tracks(artist_name, track_name):
    print >>sys.stderr, 'AS: searching for', artist_name, track_name, '\r'
    songs = []
    artists = [];
    
    
    amiestreet = AmieStreet('http://beta.amiestreet.com/api/v0.1/');
    
    results = amiestreet.SearchApi_find(artist_name, track_name);
    artists = results['artists']
    songs = results['songs']
    
    if songs[0]['found']==True:
        print >>sys.stderr, 'AS: FOUND IT', '\r'
        songObject = amiestreet.SongApi_getSongById(songs[0]['id']);
        song = {};
        song['artist']  = songObject['artist']['name']
        song['album']   = songObject['album']['title']
        song['track']   = songObject['title']
        song['source']  = 'AmieStreet.com'
        song['score']   = 1.00;
        song['url']     = 'http://amiestreet.com/stream/song'+str(songObject['id'])+'.mp3'
        return [song];
    
    print >>sys.stderr, 'AS: found nothing', '\r'
    
    return [];
 
 
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
    print get_tracks('People Under The Stairs', 'Beer');
 
#test()
####################################################################### settings
settings = dict()
settings["_msgtype"] = "settings"
settings["name"] = "Amie Street Resolver"
settings["targettime"] = 800 # millseconds
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