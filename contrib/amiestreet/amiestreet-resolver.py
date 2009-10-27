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
import sys, os
import simplejson as json
from struct import unpack, pack
from amieapi import AmieStreet

my_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(my_dir)

sys.path.insert(0, os.path.join(my_dir, '../resolver_libs/'))

import playdar_resolver



class AmieStreetResolver(playdar_resolver.PlaydarResolver):
        __client = None

        def getASApiClient(self):
            if self.__client == None:
                self.__client = AmieStreet('http://beta.amiestreet.com/api/v0.1/')

            return self.__client

        def resolver_settings(self):
            """My settings"""
            return {'name':"Amie Street Resolver", 'targettime':1000, 'weight':50}

        def results(self, query):
            results = self.getASApiClient().SearchApi_find(query['artist'], query['track']);
            if not results.has_key('songs'):
                return []

            songs = results['songs']
            
            if songs[0]['found']==True:
                songObject = self.getASApiClient().SongApi_getSongById(songs[0]['id']);
                song = {};
                song['artist']  = songObject['artist']['name']
                song['album']   = songObject['album']['title']
                song['track']   = songObject['title']
                song['source']  = 'AmieStreet.com'
                song['score']   = 1.00;
                song['url']     = 'http://amiestreet.com/stream/song'+str(songObject['id'])+'.mp3'
                song['isSample']= not songObject['actorHasSong']
                return [song]
            
            return []
                
if __name__ == "__main__":
        try:
                AmieStreetResolver.start_static()
        except:
                traceback.print_exc(file=sys.stderr)