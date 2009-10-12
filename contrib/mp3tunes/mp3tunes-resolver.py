#!/usr/bin/python
# -*- coding: utf-8 -*-
# Created by Casey Link <unnamedrambler@gmail.com> twitter.com/Ramblurr
# Based on the seeqpod resolver by:
# Max Howell <max@methylblue.com> twitter.com/mxcl
# Licensed the same as Playdar
#
# Uses Python 2.5

########################################################################
import hmac, hashlib, time
import urllib, urllib2
from xml.dom import minidom
from struct import unpack, pack
import sys
import simplejson as json
###################################################################### user settings

MP3TUNES_USER = "username" # demo@mp3tunes.com
MP3TUNES_PASS = "password" # demo
MP3TUNES_TOKEN = "partner_token"

###################################################################### static strings
MP3TUNES_LOGIN_URL = "https://shop.mp3tunes.com"
MP3TUNES_GENERAL_URL = "http://ws.mp3tunes.com"
MP3TUNES_CONTENT_URL = "http://content.mp3tunes.com"
###################################################################### functions
def print_json(o):
    s = json.dumps(o)
    sys.stdout.write(pack('!L', len(s)))
    sys.stdout.write(s)
    sys.stdout.flush()

def percent_encode(url):
    # Ha! Yeah, needs work
    return url.replace(' ', '%20')

def login():
    path = "/api/v1/login?output=xml&username="+MP3TUNES_USER+"&password="+MP3TUNES_PASS+"&partner_token="+MP3TUNES_TOKEN
    r = urllib2.Request(MP3TUNES_LOGIN_URL + path)
    response = urllib2.urlopen(r)
    doc = minidom.parseString(response.read()).getElementsByTagName('mp3tunes')
    # return:  status,session_id
    return element_value(doc[0], 'status'),element_value(doc[0], 'session_id') # Tuples! yay!

def resolve(artist, track, session):
    # only the track is searched for, because the mp3tunes search api
    # doesn't let you constrain by track && artist, only, track || artist
    # so we'll return all the results where the track matches and let playdar sort out the
    # not so good results.
    # maybe i'm a little lazy too ;)
    path = "/api/v1/lockerSearch?output=xml&partner_token="+MP3TUNES_TOKEN+"&sid="+session+"&type=track&s="+track
    r = urllib2.Request(MP3TUNES_GENERAL_URL + percent_encode(path))
    response = urllib2.urlopen(r)
    try:
        mp3tunes_node = minidom.parseString(response.read()).getElementsByTagName('mp3tunes')[0]
        track_node = mp3tunes_node.getElementsByTagName('trackList')[0]
        tracks = []
        for e in track_node.getElementsByTagName('item'):
            try:
                t = dict()
                t["artist"] = element_value(e, 'artistName')
                t["track"]  = element_value(e, 'trackTitle')
                t["album"]  = element_value(e, 'albumTitle')
                t["url"]    = percent_encode(element_value(e, 'playURL', True))
                t["source"] = 'MP3tunes'
                tracks.append(t)
            except:
                pass
        return tracks
    except:
        return []  # catch all errors and return nothing

def element_value(e, name, throw = False):
    try:
        return e.getElementsByTagName(name)[0].firstChild.nodeValue
    except:
        if throw:
            raise
        return ""

####################################################################### settings
settings = dict()
settings["_msgtype"] = "settings"
settings["name"] = "MP3tunes Resolver"
settings["targettime"] = 3000 # millseconds
settings["weight"] = 95 # mp3tunes results should be chosen just under the local collection
print_json( settings )

###################################################################### main loop
s = login()
if s[0] != "1":
    print >> sys.stderr, "MP3tunes Login Failed"
    sys.exit()  # login failed
session = s[1]
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
            tracks = resolve(request['artist'], request['track'], session)
            if len(tracks) > 0:
                response = { 'qid':request['qid'], 'results':tracks, '_msgtype':'results' }
                print_json(response)
        except:
            # safe to continue, skipping this msg, because at least
            # we consumed enough input so next iteration hits size header.
            pass

