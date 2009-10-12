#!/usr/bin/php
<?php
/*
    This is an example resolving script.

    It accepts queries on stdin, and only matches one hardcoded song,
    and replies by writing to stdout if it finds a match.

    This means you can write resolvers in any language, even PHP.
    
    The song this script matches is:
        Artist: Mokele
        Track:  Hiding in you insides

    Try searching for it using the search demo at playdar.org with this
    resolver script loaded.
*/

$in = fopen("php://STDIN",'r');

// report our settings before entering the main query loop:
send_reply( get_settings() );

// main loop: wait for a query, try and find match and reply.
while(!feof($in)){
    // get the length of the payload from the first 4 bytes:
    $len = 0;
    $lenA = unpack('N', fread($in, 4));
    $len = $lenA[1];
    if($len == 0) continue;
    // read $len bytes for the actual payload and assume it's a JSON object.
    $msg = fread($in, $len);
    $rq = json_decode($msg);
    // TODO validation - was it valid json?
    $pis = get_matches($rq);
    // don't reply with anything if there were no matches:
    if(count($pis)==0) continue;
    $res = new stdclass;
    $res->_msgtype = "results";
    $res->qid = $rq->qid;
    $res->results = array();
    $res->results = $pis;
    send_reply( $res );
}

// put a 4-byte big-endian int first, denoting length of message
function send_reply($obj){
    // i think json_spirit rejects \/ even tho it's valid json. doh.
    $str = str_replace('\/','/',json_encode($obj));
    print pack('N', strlen($str));
    print $str;
}

// this might return multiple objects if it did a proper search:
// for now it only matches one song because it's just a demo.
function get_matches($rq){
    if(strtolower(trim($rq->artist))!='mokele')  return array();
    if(soundex($rq->track)!=soundex('hiding in your insides')) return array();
    $pi = new stdclass;
    $pi->artist = "Mokele";
    $pi->track  = "Hiding In Your Insides";
    $pi->album  = "You Yourself are Me Myself and I am in Love";
    $pi->source = "Mokele.co.uk";
    $pi->size   = 4971780;
    $pi->bitrate= 160;
    $pi->duration = 248;
    $pi->extra_headers = array("X-Something: foo", "X-WTF: bar");
    // anything cURL supports:
    // NB this url should be url encoded properly:
    // $pi->url    = "ftp://user:pass@ftp.example.com/foo/bar.mp3";
    // $pi->url    = "scp://10.180.255.129/home/rj/mp3/TomWaits/Time.mp3"; // if your curl supports it!
    // $pi->url    = "file:///home/rj/mp3/Zero 7 - The Garden/Zero 7 - 11 - Crosses.mp3";
    // $pi->url    = "http://playdar:password@www.playdar.org/secret/hiding.mp3";
    $pi->url    = "http://play.mokele.co.uk/music/Hiding%20In%20Your%20Insides.mp3";
    $pi->score  = (float)1.00;
    return array($pi);
}

// settings for this resolver, reported when we start:
function get_settings(){
    $s = new stdclass;
    $s->_msgtype = "settings";
    $s->name = "php resolver script";
    $s->targettime = 20; // fast atm, it's all hardcoded.
    $s->weight = 100; // 1-100. higher means preferable.
    return $s;
}

