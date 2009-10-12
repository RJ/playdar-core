#!/usr/bin/php
<?php

require_once dirname(__FILE__) . '/phpresolver/playdarresolver.php';

/**
  * A resolver for Last.fm tracks available as free download
  * @author David Singleton (http://dsingleton.co.uk)
  */
class LastfmDownloadResolver extends PlaydarResolver
{
    protected $name = 'Last.fm Free Download';
    protected $targetTime = 15; // fast atm, it's all hardcoded.
    protected $weight = 85; // 1-100. higher means preferable.
    
    public function resolve($request)
    {
        $source = sprintf("http://last.fm/music/%s/_/%s", urlencode($request->artist), urlencode($request->track));
        $html = @file_get_contents($source); // 404 triggers a notice.
        
        preg_match('/http:\/\/freedownloads.last.fm\/.*?.mp3/s', $html, $matches);
        
        if ($matches) {
            
            $url = $matches[0];
            // re-match artist/track from title
            // get duration
            
            preg_match('/\((.:..)\)/', $html, $matches);
            if ($matches) {
                list($m, $s) = explode(":", $matches[1]);
                $duration = ($m * 60) + $s;
            
                $result = (Object) array(
                    'artist' => $request->artist,
                    'track' => $request->track,
                    'source' => 'Last.fm Free Downloads',
                    'url' => $url,
                    'bitrate' => 128,
                    'duration' => $duration,
                );
                return array($result);
            }
        }
        return array();
    }
}

$resolver = new LastfmDownloadResolver();
$resolver->sendResponse($resolver->getSettings());
$resolver->handleRequest(fopen("php://STDIN",'r'));
