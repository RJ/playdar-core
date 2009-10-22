#!/usr/bin/env ruby
# Base resolver class for Ruby
# Subclass PlaydarResolver and override #results and #resolver_settings, 
# we'll take care of the rest (even running it!)

require 'rubygems'
require 'json'

class Object
  def to_json
    JSON.dump(self)
  end
end

class String
    SoundexChars = 'BPFVCSKGJQXZDTLMNR'
    SoundexNums  = '111122222222334556'
    SoundexCharsEx = '^' + SoundexChars
    SoundexCharsDel = '^A-Z'

    # desc: http://en.wikipedia.org/wiki/Soundex
    def soundex(census = true)
        str = upcase.delete(SoundexCharsDel).squeeze

        str[0 .. 0] + str[1 .. -1].
            delete(SoundexCharsEx).
            tr(SoundexChars, SoundexNums)[0 .. (census ? 2 : -1)].
            ljust(3, '0') rescue ''
    end

    def sounds_like(other)
        soundex == other.soundex
    end
end

def print_result(object)
  json = object.to_json
  debug "Returning"
  debug json.inspect
  STDOUT.write [json.size].pack('N')
  STDOUT.write json
  STDOUT.flush
end

def debug(message)
  #STDERR.puts message
end

class PlaydarResolver
  # Override this
  def results(query)
    [{
      :artist => "Queen", 
      :track => "Crazy Little Thing Called Love",
      :size   => 4971780,
      :bitrate => 160,
      :duration => 248,
      :url => "http://127.0.0.1:8081/crazy.mp3",
      :score => 0.99
    }]
  end
  
  # Override this too
  def resolver_settings
    {}
  end
  
  def resolve(query)
    {
      :_msgtype => 'results',
      :qid => query['qid'],
      :results => results(query)
    }
  end
  
  def settings
    {
      :_msgtype => "settings",
      :name => "Generic Ruby Resolver",
      :targettime => 5000,
      :weight => 100
    }.merge(resolver_settings)
  end
  
  def start
    print_result(settings)
    
    catch :done do
      loop while true
    end
  end
  
  def self.start!
    resolver = self.new
    resolver.start
  end
  
  def self.inherited(subclass)
    at_exit{ subclass.start! } unless __FILE__ == $0
  end
  
  def loop
    throw :done unless len_bytes = STDIN.read(4)
    debug "len_bytes #{len_bytes.inspect}"
    throw :done unless length = len_bytes.unpack('N')[0]
    debug "length #{length.inspect}"
    throw :done if length > 4096 || length < 0

    if length > 0
      msg = STDIN.read(length)
      debug "msg #{msg.inspect}"
      begin
        request = JSON.parse(msg)
        result = resolve(request)
        print_result(result)
      rescue Exception => e
        debug "#{e}\n#{e.backtrace.join("\n")}"
      end
    end
  end
end
