#!/usr/bin/env ruby
# In your etc/playdar.conf, add "contrib/soundcloud/soundcloud_resolver.rb"
# to see this in action.

require File.dirname(__FILE__) + '/../resolver_libs/playdar_resolver'

require 'rubygems'
gem 'soundcloud-ruby-api-wrapper'
require 'soundcloud'

class SoundCloudResolver < PlaydarResolver
  def resolver_settings()
    {
      :name => "SoundCloud Ruby Resolver"
    }
  end
  
  def results(query)
    query = query.symbolize_keys
    
    soundcloud = Soundcloud.register
    query.slice(:artist, :title).values.compact.uniq.map do |value|
      soundcloud.Track.find(:all, :from => "/tracks?q=#{value}&streamable=true")
    end.flatten.map do |track|
      response = {
        # Required
        :url      => track.stream_url,
        :source   => "SoundCloud",
        :track    => track.title,
        :artist   => track.attributes['user'].attributes['username'],
        
        # Highly recommended
        :duration => track.duration.to_i / 1000,
        :album    => "SoundCloud",
        :bitrate  => 128,
        
        # Recommended
        # :size     => -1,
        # :mimetype => ...
      }
      
      response.merge({
        :score => [ :artist, :title ].inject(0.5) { |sum, key| (response[key] || "").casecmp(query[key] || "") == 0 ? sum + 0.25 : sum }
      })
    end
  end
end
