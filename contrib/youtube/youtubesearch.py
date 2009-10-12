#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Casey Link
# Author: Ricardo Garcia Gonzalez
# Author: Danny Colligan
# http://bitbucket.org/rg3/youtube-dl/wiki/Home
# License: Public domain code
import htmlentitydefs
import httplib
import locale
import math
import netrc
import os
import os.path
import re
import socket
import string
import sys
import time
import urllib
import urllib2

std_headers = {
    'User-Agent': 'Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.8) Gecko/2009032609 Firefox/3.0.8',
    'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.7',
    'Accept': 'text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5',
    'Accept-Language': 'en-us,en;q=0.5',
}

simple_title_chars = string.ascii_letters.decode('ascii') + string.digits.decode('ascii')

class UnavailableFormatError(Exception):
    """Unavailable Format exception.

    This exception will be thrown when a video is requested
    in a format that is not available for that video.
    """

class InfoExtractor(object):
    """Information Extractor class.

    Information extractors are the classes that, given a URL, extract
    information from the video (or videos) the URL refers to. This
    information includes the real video URL, the video title and simplified
    title, author and others. The information is stored in a dictionary
    which is then passed to the FileDownloader. The FileDownloader
    processes this information possibly downloading the video to the file
    system, among other possible outcomes. The dictionaries must include
    the following fields:

    id:     Video identifier.
    url:        Final video URL.
    uploader:   Nickname of the video uploader.
    title:      Literal title.
    stitle:     Simplified title.
    ext:        Video filename extension.

    Subclasses of this one should re-define the _real_initialize() and
    () methods, as well as the suitable() static method.
    Probably, they should also be instantiated and added to the main
    downloader.
    """

    _ready = False
    _downloader = None

    def __init__(self, downloader=None):
        """Constructor. Receives an optional downloader."""
        self._ready = False
        self.set_downloader(downloader)

    @staticmethod
    def suitable(url):
        """Receives a URL and returns True if suitable for this IE."""
        return False

    def initialize(self):
        """Initializes an instance (authentication, etc)."""
        if not self._ready:
            self._real_initialize()
            self._ready = True

    def extract(self, url):
        """Extracts URL information and returns it in list of dicts."""
        self.initialize()
        return self._real_extract(url)

    def set_downloader(self, downloader):
        """Sets the downloader for this IE."""
        self._downloader = downloader

    def _real_initialize(self):
        """Real initialization process. Redefine in subclasses."""
        pass

    def _real_extract(self, url):
        """Real extraction process. Redefine in subclasses."""
        pass

class YoutubeIE(InfoExtractor):
    """Information extractor for youtube.com."""

    _VALID_URL = r'^((?:http://)?(?:\w+\.)?youtube\.com/(?:(?:v/)|(?:(?:watch(?:\.php)?)?\?(?:.+&)?v=)))?([0-9A-Za-z_-]+)(?(1).+)?$'
    _LANG_URL = r'http://uk.youtube.com/?hl=en&persist_hl=1&gl=US&persist_gl=1&opt_out_ackd=1'
    _LOGIN_URL = 'http://www.youtube.com/signup?next=/&gl=US&hl=en'
    _AGE_URL = 'http://www.youtube.com/verify_age?next_url=/&gl=US&hl=en'
    _NETRC_MACHINE = 'youtube'
    _available_formats = ['22', '35', '18', '17', '13'] # listed in order of priority for -b flag
    _video_extensions = {
        '13': '3gp',
        '17': 'mp4',
        '18': 'mp4',
        '22': 'mp4',
    }

    @staticmethod
    def suitable(url):
        return (re.match(YoutubeIE._VALID_URL, url) is not None)

    @staticmethod
    def htmlentity_transform(matchobj):
        """Transforms an HTML entity to a Unicode character."""
        entity = matchobj.group(1)

        # Known non-numeric HTML entity
        if entity in htmlentitydefs.name2codepoint:
            return unichr(htmlentitydefs.name2codepoint[entity])

        # Unicode character
        mobj = re.match(ur'(?u)#(x?\d+)', entity)
        if mobj is not None:
            numstr = mobj.group(1)
            if numstr.startswith(u'x'):
                base = 16
                numstr = u'0%s' % numstr
            else:
                base = 10
            return unichr(long(numstr, base))

        # Unknown entity in name, return its literal representation
        return (u'&%s;' % entity)

    def report_lang(self):
        """Report attempt to set language."""
        #self._downloader.to_stdout(u'[youtube] Setting language')

    def report_login(self):
        """Report attempt to log in."""
        #self._downloader.to_stdout(u'[youtube] Logging in')

    def report_age_confirmation(self):
        """Report attempt to confirm age."""
        #self._downloader.to_stdout(u'[youtube] Confirming age')

    def report_webpage_download(self, video_id):
        """Report attempt to download webpage."""
        #self._downloader.to_stdout(u'[youtube] %s: Downloading video webpage' % video_id)

    def report_information_extraction(self, video_id):
        """Report attempt to extract video information."""
        #self._downloader.to_stdout(u'[youtube] %s: Extracting video information' % video_id)

    def report_video_url(self, video_id, video_real_url):
        """Report extracted video URL."""
        #self._downloader.to_stdout(u'[youtube] %s: URL: %s' % (video_id, video_real_url))

    def report_unavailable_format(self, video_id, format):
        """Report extracted video URL."""
        #self._downloader.to_stdout(u'[youtube] %s: Format %s not available' % (video_id, format))

    def _real_initialize(self):
        if self._downloader is None:
            return

        username = None
        password = None
        downloader_params = self._downloader.params

        # Attempt to use provided username and password or .netrc data
        if downloader_params.get('username', None) is not None:
            username = downloader_params['username']
            password = downloader_params['password']
        elif downloader_params.get('usenetrc', False):
            try:
                info = netrc.netrc().authenticators(self._NETRC_MACHINE)
                if info is not None:
                    username = info[0]
                    password = info[2]
                else:
                    raise netrc.NetrcParseError('No authenticators for %s' % self._NETRC_MACHINE)
            except (IOError, netrc.NetrcParseError), err:
                self._downloader.to_stderr(u'WARNING: parsing .netrc: %s' % str(err))
                return

        # Set language
        request = urllib2.Request(self._LANG_URL, None, std_headers)
        try:
            self.report_lang()
            urllib2.urlopen(request).read()
        except (urllib2.URLError, httplib.HTTPException, socket.error), err:
            self._downloader.to_stderr(u'WARNING: unable to set language: %s' % str(err))
            return

        # No authentication to be performed
        if username is None:
            return

        # Log in
        login_form = {
                'current_form': 'loginForm',
                'next':     '/',
                'action_login': 'Log In',
                'username': username,
                'password': password,
                }
        request = urllib2.Request(self._LOGIN_URL, urllib.urlencode(login_form), std_headers)
        try:
            self.report_login()
            login_results = urllib2.urlopen(request).read()
            if re.search(r'(?i)<form[^>]* name="loginForm"', login_results) is not None:
                self._downloader.to_stderr(u'WARNING: unable to log in: bad username or password')
                return
        except (urllib2.URLError, httplib.HTTPException, socket.error), err:
            self._downloader.to_stderr(u'WARNING: unable to log in: %s' % str(err))
            return

        # Confirm age
        age_form = {
                'next_url':     '/',
                'action_confirm':   'Confirm',
                }
        request = urllib2.Request(self._AGE_URL, urllib.urlencode(age_form), std_headers)
        try:
            self.report_age_confirmation()
            age_results = urllib2.urlopen(request).read()
        except (urllib2.URLError, httplib.HTTPException, socket.error), err:
            self._downloader.trouble(u'ERROR: unable to confirm age: %s' % str(err))
            return

    def _real_extract(self, url):
        # Extract video id from URL
        mobj = re.match(self._VALID_URL, url)
        if mobj is None:
            print('ERROR: invalid URL: %s' % url)
            return
        video_id = mobj.group(2)

        # Downloader parameters
        best_quality = False
        format_param = None
        quality_index = 0
        if self._downloader is not None:
            params = self._downloader.params
            format_param = params.get('format', None)
            if format_param == '0':
                format_param = self._available_formats[quality_index]
                best_quality = True

        while True:
            try:
                # Extension
                video_extension = self._video_extensions.get(format_param, 'flv')

                # Normalize URL, including format
                normalized_url = 'http://www.youtube.com/watch?v=%s&gl=US&hl=en' % video_id
                if format_param is not None:
                    normalized_url = '%s&fmt=%s' % (normalized_url, format_param)
                request = urllib2.Request(normalized_url, None, std_headers)
                try:
#                    self.report_webpage_download(video_id)
                    video_webpage = urllib2.urlopen(request).read()
                except (urllib2.URLError, httplib.HTTPException, socket.error), err:
                    self._downloader.trouble(u'ERROR: unable to download video webpage: %s' % str(err))
                    return
                #self.report_information_extraction(video_id)

                # "t" param
                mobj = re.search(r', "t": "([^"]+)"', video_webpage)
                if mobj is None:
                    self._downloader.trouble(u'ERROR: unable to extract "t" parameter')
                    return
                video_real_url = 'http://www.youtube.com/get_video?video_id=%s&t=%s&el=detailpage&ps=' % (video_id, mobj.group(1))
                if format_param is not None:
                    video_real_url = '%s&fmt=%s' % (video_real_url, format_param)
 #               self.report_video_url(video_id, video_real_url)

                # uploader
                mobj = re.search(r"var watchUsername = '([^']+)';", video_webpage)
                if mobj is None:
                    self._downloader.trouble(u'ERROR: unable to extract uploader nickname')
                    return
                video_uploader = mobj.group(1)

                # title
                mobj = re.search(r'(?im)<title>YouTube - ([^<]*)</title>', video_webpage)
                if mobj is None:
                    self._downloader.trouble(u'ERROR: unable to extract video title')
                    return
                video_title = mobj.group(1).decode('utf-8')
                video_title = re.sub(ur'(?u)&(.+?);', self.htmlentity_transform, video_title)
                video_title = video_title.replace(os.sep, u'%')

                # simplified title
                simple_title = re.sub(ur'(?u)([^%s]+)' % simple_title_chars, ur'_', video_title)
                simple_title = simple_title.strip(ur'_')

                # Process video information
                info = {
                    'id':       video_id.decode('utf-8'),
                    'url':      video_real_url.decode('utf-8'),
                    'uploader': video_uploader.decode('utf-8'),
                    'title':    video_title,
                    'stitle':   simple_title,
                    'ext':      video_extension.decode('utf-8'),
                }
                return info

            except UnavailableFormatError, err:
                if best_quality:
                    if quality_index == len(self._available_formats) - 1:
                        # I don't ever expect this to happen
                        self._downloader.trouble(u'ERROR: no known formats available for video')
                        return
                    else:
                        self.report_unavailable_format(video_id, format_param)
                        quality_index += 1
                        format_param = self._available_formats[quality_index]
                        continue
                else:
                    self._downloader.trouble('ERROR: format not available for video')
                    return

class YoutubeSearchIE(InfoExtractor):
    """Information Extractor for YouTube search queries."""
    _VALID_QUERY = r'ytsearch(\d+|all)?:[\s\S]+'
    _TEMPLATE_URL = 'http://www.youtube.com/results?search_query=%s&page=%s&gl=US&hl=en'
    _VIDEO_INDICATOR = r'href="/watch\?v=.+?"'
    _MORE_PAGES_INDICATOR = r'>Next</a>'
    _youtube_ie = None
    _max_youtube_results = 1000

    def __init__(self, youtube_ie, downloader=None):
        InfoExtractor.__init__(self, downloader)
        self._youtube_ie = youtube_ie

    @staticmethod
    def suitable(url):
        return (re.match(YoutubeSearchIE._VALID_QUERY, url) is not None)

    def report_download_page(self, query, pagenum):
        """Report attempt to download playlist page with given number."""
        self._downloader.to_stdout(u'[youtube] query "%s": Downloading page %s' % (query, pagenum))

    def _real_initialize(self):
        self._youtube_ie.initialize()

    def _real_extract(self, query):
        mobj = re.match(self._VALID_QUERY, query)
        if mobj is None:
            self._downloader.trouble(u'ERROR: invalid search query "%s"' % query)
            return

        prefix, query = query.split(':')
        prefix = prefix[8:]
        if prefix == '':
            return self._download_n_results(query, 1)
        else:
            return

    def _download_n_results(self, query, n):
        """Downloads a specified number of results for a query"""

        video_ids = []
        already_seen = set()
        pagenum = 1

        while True:
           # self.report_download_page(query, pagenum)
            result_url = self._TEMPLATE_URL % (urllib.quote_plus(query), pagenum)
            request = urllib2.Request(result_url, None, std_headers)
            try:
                page = urllib2.urlopen(request).read()
            except (urllib2.URLError, httplib.HTTPException, socket.error), err:
                self._downloader.trouble(u'ERROR: unable to download webpage: %s' % str(err))
                return

            # Extract video identifiers
            for mobj in re.finditer(self._VIDEO_INDICATOR, page):
                video_id = page[mobj.span()[0]:mobj.span()[1]].split('=')[2][:-1]
                if video_id not in already_seen:
                    video_ids.append(video_id)
                    already_seen.add(video_id)
                    if len(video_ids) == n:
                        # Specified n videos reached
                        #for id in video_ids:
                            return self._youtube_ie.extract('http://www.youtube.com/watch?v=%s' % video_ids[0])
                        

            #if self._MORE_PAGES_INDICATOR not in page:
             #   for id in video_ids:
              #      self._youtube_ie.extract('http://www.youtube.com/watch?v=%s' % id)
               # return

            pagenum = pagenum + 1