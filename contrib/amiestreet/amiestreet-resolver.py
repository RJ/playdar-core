#!/usr/bin/python
# -*- coding: utf-8 -*-
# Amie Street resolver by Lucas Hrabovsky<hrabovsky.lucas@gmail.com> twitter.com/__lucas
# Using the Amie Street API Python Client available at http://github.com/imlucas/amiestreet-api/blob/master/py/amie-api.py
#
# Based on the echonest resolver by:
# Paul Lamere<Paul.Lamere@gmail.com> twitter.com/plamere
#
# Based on the seeqpod resolver by:
# Max Howell <max@methylblue.com> twitter.com/mxcl
# Licensed the same as Playdar
#
# pyechonest available at http://code.google.com/p/pyechonest/
# http://bitbucket.org/rg3/youtube-dl/wiki/Home
 
########################################################################
import sys
import simplejson as json
from struct import unpack, pack
from amieapi import AmieStreet

##########################################
# Amie Street Resolver
##########################################
def get_tracks(artist_name, track_name):
    print >>sys.stderr, 'Amie Street: searching for', artist_name, track_name, '\r'
    songs = []
    amiestreet = AmieStreet('http://beta.amiestreet.com/api/v0.1/');
    results = amiestreet.SearchApi_find(artist_name, track_name);
    songs = results['songs']
    
    if songs[0]['found']==True:
        print >>sys.stderr, 'Amire Street: FOUND IT', '\r'
        songObject = amiestreet.SongApi_getSongById(songs[0]['id']);
        song = {};
        song['artist']  = songObject['artist']['name']
        song['album']   = songObject['album']['title']
        song['track']   = songObject['title']
        song['source']  = 'AmieStreet.com'
        song['score']   = 1.00;
        song['url']     = 'http://amiestreet.com/stream/song'+str(songObject['id'])+'.mp3'
        song['isSample']= not songObject['actorHasSong']
        return [song];
    
    print >>sys.stderr, 'Amie Street: found nothing', '\r'
    
    return [];

###################################################################### functions
def print_json(o):
    s = json.dumps(o)
    sys.stdout.write(pack('!L', len(s)))
    sys.stdout.write(s)
    sys.stdout.flush()
 
def resolve(artist, track):
    return get_tracks(artist, track)
 
 
def test():
    print get_tracks('People Under The Stairs', 'Beer');

####################################################################### settings
settings = dict()
settings["_msgtype"] = "settings"
settings["name"] = "Amie Street Resolver"
settings["targettime"] = 800
settings["weight"] = 50
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