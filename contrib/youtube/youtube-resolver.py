#!/usr/bin/python
# -*- coding: utf-8 -*-
# Created by Casey Link <unnamedrambler@gmail.com> twitter.com/Ramblurr
# Based on the seeqpod resolver by:
# Max Howell <max@methylblue.com> twitter.com/mxcl
# Licensed the same as Playdar
#
# Uses Python 2.5 and youtube-dl a public domain youtube downloader
# http://bitbucket.org/rg3/youtube-dl/wiki/Home

########################################################################
import hmac, hashlib, time
import urllib, urllib2
from xml.dom import minidom
from struct import unpack, pack
import sys
sys.path.insert(0, '/home/rj/src/playdar/scripts/youtube')
import simplejson as json
from youtubesearch import YoutubeIE
from youtubesearch import YoutubeSearchIE

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

    querystr = "ytsearch:" + artist + " - " + track
    youtube_ie = YoutubeIE()
    youtube_search_ie = YoutubeSearchIE(youtube_ie)
    info = youtube_search_ie.extract(querystr)
    tracks = []
    t = dict()
    t["artist"] = artist
    t["track"]  = track 
    t["album"]  = "Unknown"
    # thru our lame transcoder to mp3:
    t["url"]    = "http://localhost:8080/" + info["url"]
    t["source"] = 'Youtube'
    tracks.append(t)
    return tracks

####################################################################### settings
settings = dict()
settings["_msgtype"] = "settings"
settings["name"] = "Youtube Resolver"
settings["targettime"] = 5000 # millseconds
settings["weight"] = 50 # youtube results aren't as good as friend's results
print_json( settings )

###################################################################### main loop
while 1:
    length = sys.stdin.read(4)
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
            tracks = resolve(request['artist'], request['track'])
            if len(tracks) > 0:
                response = { 'qid':request['qid'], 'results':tracks, '_msgtype':'results' }
                print_json(response)
        except:
            # safe to continue, skipping this msg, because at least
            # we consumed enough input so next iteration hits size header.
            pass

