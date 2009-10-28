#!/usr/bin/python
# -*- coding: utf-8 -*-
# Sound Cloud resolver by Lucas Hrabovsky<hrabovsky.lucas@gmail.com> twitter.com/__lucas
 
########################################################################
import sys, os, getopt, urllib2, urllib

import simplejson as json
from struct import unpack, pack

my_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(my_dir)
sys.path.insert(0, os.path.join(my_dir, '../resolver_libs/'))

import playdar_resolver

class SoundCloudResolver(playdar_resolver.PlaydarResolver):
        def resolver_settings(self):
            """My settings"""
            return {'name':"Sound Cloud Resolver", 'targettime':5000, 'weight':50}

        def results(self, query):
            param_string = ''
            params = {}
            params['filter']="streamable"
            params['q'] = '"'+query['artist']+' '+query['track']+'"';
            param_string = urllib.urlencode(params)

            url = 'http://soundcloud.com/api/tracks.json?'+param_string

            c=urllib2.urlopen(url)
            contents = c.read()
            print url
            print contents
            try:
                data = json.read(contents)
            except ValueError:
                return []

            if len(data) < 1:
                return []
            
            i =0
            song = {};
            #song['artist']  = 
            #song['album']   = songObject['album']['title']
            song['track']   = data[i]['title']
            song['source']  = 'Sound Cloud'
            song['score']   = 1.00;
            song['url']     = data[i]['stream_url']
            song['duration']= data[i]['duration']
            #song['download_url'] = data[i]['download_url']
            return [song];
        def test(self):
            print self.results({'artist':"Moby", 'track':"James Bond Theme"})
                
if __name__ == "__main__":
    try:                                
        opts, args = getopt.getopt(sys.argv[1:], "t", ["test"])
        for opt, arg in opts:
            if opt in ("-t", "--test"):
                r = SoundCloudResolver()
                r.test()                   
                sys.exit()                    
    except getopt.GetoptError:       
        sys.exit(2)                     

    SoundCloudResolver.start_static()