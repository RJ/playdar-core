#!/usr/bin/php
<?php

$in = fopen("php://STDIN", "r");

// report our settings before entering the main query loop:
send_reply( get_settings() );

// load index:
$magnatune = @require("magnatune-idx.php");

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
    global $magnatune;
    $tracks = @$magnatune[metaphone($rq->artist)][metaphone($rq->track)];
    if(!$tracks) return array();
    $ret = array();
    foreach($tracks as $t){
        $pi = new stdclass;
        $pi->artist = $t['artist'];
        $pi->track  = $t['track'];
        $pi->album  = $t['album'];
        $pi->source = "Magnatune.com";
//      $pi->size   = 0;
        $pi->bitrate= 128;
        $pi->duration = (int)$t['duration'];
        $pi->url    = $t['url'];
        $pi->score  = (float)1.00;
        $ret[]=$pi;
    }
    return $ret;
}

// settings for this resolver, reported when we start:
function get_settings(){
    $s = new stdclass;
    $s->_msgtype = "settings";
    $s->name = "magnatune script";
    $s->targettime = 30; // fast atm, it's all hardcoded.
    $s->weight = 100; // 1-100. higher means preferable.
    return $s;
}

