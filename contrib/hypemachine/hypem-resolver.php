#!/usr/bin/php
<?php

require_once dirname(__FILE__) . '/phpresolver/playdarresolver.php';

/**
  * A resolver for MP3s from blogs indexed by Hype Machine
  * @author David Singleton (http://dsingleton.co.uk)
  * @todo Get the cookie once only, cache and re-send.
  */
class HypeMachineResolver extends PlaydarResolver
{
    protected $name = 'HypeMachine Resolver';
    protected $targetTime = 5000; // Rough guess for a remote service
    protected $weight = 60;
    
    private $cookie = array();
    
    public function resolve($request)
    {
        $source = sprintf('http://hypem.com/search/"%s"+"%s"/', urlencode($request->artist), urlencode($request->track));
        // $html = file_get_contents("test/hypem.html");
        // We need to grab the AUTH cookie we get issued here.
        $ch = curl_init();
        
        curl_setopt($ch, CURLOPT_URL, $source);
        curl_setopt($ch, CURLOPT_HEADER, 1);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1); 
        curl_setopt($ch, CURLOPT_USERAGENT, 'Playdar 0.1');
        
        $x = curl_exec($ch);
        curl_close($ch);
        
        if (!$x) {
            return false;
        }   
        
        $headers = array();
        
        list($head, $html) = explode("\r\n\r\n", $x, 2);
        
        $aHeaders = explode("\r\n", $head);
        array_shift($aHeaders);
        
        foreach($aHeaders as $header) {
            list($key, $val) = explode(": ", $header);
            $headers[$key] = $val;
        }
        
        if (!($headers && isset($headers['Set-Cookie']))) {
            return false;
        }
        
        list($cookies) = explode(";", $headers['Set-Cookie']);
        list($key, $val) = explode("=", $cookies);
        $aCookies = array($key => $val);
    
        if (!isset($aCookies['AUTH'])) {
            return false;
        }
        
        $auth = $aCookies['AUTH'];
        

        preg_match_all('/\({.*?}\)/s', $html, $matches);
        $results = array();
        foreach($matches[0] as $match) {
            // We might want to filter out some of the fields, they're mostly useless.
            $json = $this->reformatHypeJSON(substr($match, 2, -2));
            $result = json_decode($json);
            if (!$result) {
                continue;
            }
            $result->url = sprintf("http://hypem.com/serve/play/%s/%s", $result->id, $result->key);
            $result->track = $result->song;

            $result->duration = (int) $result->time;
            $result->extra_headers = array("Cookie: AUTH=$auth");
            $result->source = "Hype Machine";
            // Use the unique id as a fraction to vary keys where the score is the same.
            $key =  round($result->score) . "-{$result->id}";
            
            $result->bitrate = 128;
            $results[$key] = $result;
        }
        
        krsort($results, SORT_NUMERIC);
        $results = array_values(array_slice($results, 0, 2));
        
        return $results;
    }
    
    private function reformatHypeJSON($hypeJSON)
    {
        // Mung and fix json, convert to obj
        
        $pairs = explode("\n", trim($hypeJSON));
        $json = '';
        
        foreach($pairs as $pair) {
            list($key, $val) = explode(":", trim($pair));
            
            $key = trim($key);
            $val = trim($val);
            
            $key = "\"$key\":";
            $val = substr($val, 1, (strrpos($val, ',') === strlen($val)-1 ? -2 : -1));

            $json .=  $key . "\"$val\"" . ',';
        }

        $json = substr($json, 0, -1);
        return '{' . $json . '}';
    }
}

$resolver = new HypeMachineResolver();
$resolver->sendResponse($resolver->getSettings());
$resolver->handleRequest(fopen("php://STDIN",'r'));
