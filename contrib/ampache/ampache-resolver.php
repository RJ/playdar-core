#!/usr/bin/env php
<?php

require dirname(__FILE__) . '/../phpresolver/playdarresolver.php';

class AmpacheResolver extends PlaydarResolver {
    protected $name = 'ampache resolver';
    protected $targetTime = 50;
    protected $weight = 100;
    protected $localonly = FALSE;
  
    private $link;
    private $prefix;
    
    function __construct($db){
      parent::__construct();
      
      $this->link = mysql_connect($db['host'], $db['user'], $db['password']) or die('Could not connect to MySQL database');
      mysql_select_db($db['db'], $this->link);
      mysql_query('SET NAMES utf8', $this->link);
    }
    
    function query($sql){
      $result = mysql_query($sql, $this->link);
      if (mysql_errno($this->link))
        exit('MySQL error ' . mysql_errno($this->link) . ': ' . mysql_error() . "\n");
      
      if (!$result)
        return array();
        
      $items = array();
      while ($item = mysql_fetch_array($result))
        $items[] = $item;
                
      return $items;
    }
        
    public function resolve($request) {          
      $where = array();
      foreach (array('artist' => 'artist.name', 'album' => 'album.name', 'track' => 'song.title') as $key => $value)
        if (!empty($request->{$key}))
          $where[] = sprintf("%s = '%s'", $value, mysql_real_escape_string($request->{$key}));
      $where = implode(' AND ', $where);
      
      $sql = "SELECT artist.name as artist, song.title as track, album.name as album, song.size, song.bitrate, song.time as duration, CONCAT('file://', song.file) as url
      FROM song LEFT JOIN album on song.album = album.id LEFT JOIN artist on song.artist = artist.id 
      WHERE $where LIMIT 100";
      
      $items =  $this->query($sql);
      
      if (!empty($items)){
        foreach ($items as &$item){            
          $item['score'] = 1; // TODO
          $item['source'] = 'Ampache';
        }
      }
      
      return $items;
    }
}

require dirname(__FILE__) . '/ampache-resolver.conf';
$resolver = new AmpacheResolver($db, $prefix);

//exit(print_r($resolver->resolve((object) array('artist' => 'Yeah Yeah Yeahs', 'track' => 'Heads Will Roll'))));

$resolver->sendResponse($resolver->getSettings());
$resolver->handleRequest(fopen("php://STDIN",'r'));

