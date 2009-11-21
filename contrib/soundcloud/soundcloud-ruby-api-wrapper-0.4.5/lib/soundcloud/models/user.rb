module Soundcloud
  module Models
    # SC API Attributes (as of 26/05/09):
    # * id
    # * username
    # * full_name
    # * description
    # * city
    # * country
    # * discogs_name
    # * myspace_name
    # * website
    # * website_title
    # * online
    # * avatar_url
    # * permalink_url
    # * uri
    # * track_count
    #
    # Custom Wrapper Attributes/Methods:
    # * tracks
    # * contacts
    # * comments
    # * favorites
    # * playlists
    # * fans
    # * is_contact?
    # * add_contact!
    # * remove_contact!
    # * has_favorite?(track/track_id)
    # * has_contact?(user/user_id)
    #    
    # Look up the resource attributes and filtering usage here:
    #        
    # http://wiki.github.com/soundcloud/api/documentation#user
    #
    # Custom Wrapper Class Methods
    # * find_me 
    #
    # Examples:
    #   # gets the logged-in user
    #   me = client.User.find_me
    #    
    #   # gets the user with username userABC
    #   user = sc_client.User.find('userABC')
    #  
    #   # finds all users named joe and print their usernames    
    #   joes = sc_client.User.find(:all, :params=> {:q => "joe"})
    #   joes.each do |user|
    #     p user.username
    #   end
    #   
    #
    #   # checks if the first user named joe is following the second user named joe
    #   joe1 = joes.first
    #   joe2 = joes[1]
    #   joe1.has_contact?(joe2)
    #
    #   # makes the loggedin user following joe2
    #   joe2.add_contact!
    #
    #
    #   # Display 50 (Soundcloud API limit) tracks of a user
    #   user = sc_client.User.find('some-user')
    #   user.tracks.each do |track|
    #     p  track.title
    #   end
    #
    #   # Get all fans of a user
    #   fans = []
    #   limit = 50
    #   begin 
    #     some_fans = famous_dj.fans({:offset => fans.length, :limit => limit})
    #     fans += some_fans
    #   end while some_fans.length >= limit
    #
    #
    
    class User < Base
      has_many :tracks, :contacts, :comments, :favorites, :playlists, :fans
      has_many_single_changeable :contacts, :favorites
      can_be_a_single_changeable :contact
      
      cattr_accessor :element_name      
      self.element_name = 'user'

      # Convenience method to find the logged in user
      def self.find_me
        find(:one, :from => '/me')
      end
    end
    
    class Permission < User #:nodoc: 
    end
    
    class Contact < User #:nodoc:
    end
    
    class Fan < User #:nodoc:
    end
        
    class Creator < User #:nodoc:
    end
    
    class Contributor < User
    end
    
    class Member < User #:nodoc:
    end
    
    class Moderator < User #:nodoc:
    end
  end
end

