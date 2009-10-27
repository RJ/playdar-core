#!/usr/bin/env ruby
# In your etc/playdar.conf, add "contrib/resolver_libs/example_resolver.rb"
# to see this in action.
require File.dirname(__FILE__) + '/playdar_resolver'

class ExampleResolver < PlaydarResolver
  def resolver_settings() {:name => "Example Ruby Resolver"} end
  def results(query)
    return [] unless query['artist'].downcase == 'mokele'
    return [] unless query['track'].sounds_like('hiding in your insides')
    
    [{
      :artist => "Mokele",
      :track => "Hiding In Your Insides (ruby)",
      :album => "You Yourself are Me Myself and I am in Love",
      :source => "Mokele.co.uk",
      :size => 4971780,
      :bitrate => 160,
      :duration => 248,
      # NB this url should be url encoded properly:
      :url => "http://play.mokele.co.uk/music/Hiding%20In%20Your%20Insides.mp3",
      :score => 1.00
    }]
  end
end