module Soundcloud
  module Models
    #
    # SC API Attributes (as of 26/05/09): 
    # * type
    # * id
    # * created_at
    # * resource_id
    # * comment/track/user (embedded resource, depending on type)
    #
    # Custom Wrapper Attribute:
    # * event_type (access to type attribute, since 'type' is a ruby keyword)
    #
    # Look up the resource attributes and filtering usage here:
    #    
    # http://wiki.github.com/soundcloud/api/documentation#event
    #
    # Examples:
    #
    #   # find the last 50 (default soundcloud limit) dropbox events and display the titles of the dropped tracks
    #   dropbox_events = sc_client.Event.find(:all,:params => {:filter => 'drop'})
    #
    #   dropbox_events.each do |event|
    #     p event.track.title
    #   end
    #    
    #   # find the last 50 (default soundcloud limit)  events    
    #   sc_client.Event.find(:all)
    #
    class Event < Base    
      cattr_accessor :element_name    
      self.element_name = 'event'
      
      
      def event_type 
        return attributes['type']
      end
    end    
  end
end
