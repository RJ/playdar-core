#!/usr/bin/env php
<?php

require dirname(__FILE__) . '/../phpresolver/playdarresolver.php';

class AmazonResolver extends PlaydarResolver {
    protected $name = 'Amazon MP3 Preview resolver';
    protected $targetTime = 500;
    protected $weight = 20;
    protected $localonly = TRUE;
  
    private $key;
    private $secret;
    
    private $host = 'ecs.amazonaws.com';
    private $path = '/onca/xml';
    private $tag = 'amazonbrowser-20';
    
    function __construct($conf){
      parent::__construct();
      
      foreach ($conf as $key => $value)
        $this->{$key} = $value;
    }
    
    function sign($params){
      uksort($params, 'strnatcmp');
      foreach ($params as $key => &$value)
        $value = $key . '=' . rawurlencode($value); // http_build_query uses urlencode rather than rawurlencode

      return base64_encode(hash_hmac('sha256', implode("\n", array('GET', $this->host, $this->path, implode('&', $params))), $this->secret, TRUE));
    }
    
    function resolve($request) {
      $query = implode(' - ', array($request->artist, $request->album, $request->track));
      
      $params = array(
        'Service' => 'AWSECommerceService',
        'Version' => '2009-10-01',
        'Timestamp' => date(DATE_ISO8601),
        'AWSAccessKeyId' => $this->key,
        'AssociateTag' => $this->tag,
        'Operation' => 'ItemSearch',
        'SearchIndex' => 'MP3Downloads',
        'ResponseGroup' => 'ItemAttributes,Images',
        'Keywords' => $query,
        );

      $params['Signature'] = $this->sign($params);     
      $xml = simplexml_load_file('http://' . $this->host . $this->path . '?' . http_build_query($params));
      //print_r($xml);
      
      if (!is_object($xml) || (string) $xml->Items->Request->IsValid != 'True')
        return array();

      $items = array();
      if (!empty($xml->Items->Item)){
        foreach ($xml->Items->Item as $item){
          $asin = (string) $item->ASIN;
          $attr = $item->ItemAttributes;
          
          $meta = array(
            'score' => 1,
            'source' => 'Amazon',
            'artist' => (string) $attr->Creator,
            'track' => (string) $attr->Title . ' [preview]',
            'duration' => 30, // (string) $attr->RunningTime,
            'trackno' => (string) $attr->TrackSequence,
            'url' => 'http://www.amazon.com/gp/dmusic/get_sample_url.html?ASIN=' . $asin,
            'info' => (string) $item->DetailPageURL,
            );
            
          foreach (array('LargeImage', 'MediumImage', 'SmallImage') as $image){
            if (isset($item->{$image})){
              $meta['image'] = (string) $item->{$image}->URL;
              break;
            }
          }
            
          $items[] = $meta;
        }
      }   
      return $items;
    }
}

require dirname(__FILE__) . '/amazon-resolver.conf';
$resolver = new AmazonResolver($conf);

//exit(print_r($resolver->resolve((object) array('artist' => 'Yeah Yeah Yeahs', 'track' => 'Heads Will Roll'))));

$resolver->sendResponse($resolver->getSettings());
$resolver->handleRequest(fopen("php://STDIN",'r'));

