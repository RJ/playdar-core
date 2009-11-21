module Soundcloud
  module Models   
    # 
    # SC API Attributes (as of 26/05/09):
    # * body
    # * timestamp
    # * id
    # * track_id
    # * user_id
    # * uri
    #
    # Custom Wrapper Attributes/Methods:
    # * user 
    # * track
    #
    # Look up the resource attributes and filtering usage here:
    #         
    # http://wiki.github.com/soundcloud/api/documentation#comment
    #
    # Examples:
    # 
    #   # add a comment to a track
    #   some_track = sc_client.Track.find('some-track')
    #   sc_client.Comment.create({:track => some_track, :body => 'Nice Track!'})
    #  
    #   # optionally you can add a timestamp (in milliseconds)
    #   sc_client.Comment.create({:track => some_track, :body => 'nice drums @ the 5th second!', :timestamp => 5000})
    #
    #   # display all comments of some track
    #   some_track.comments.each do |comment| 
    #     p "#{comment.user.full_name} wrote: #{comment.body}"
    #   end  

    class Comment < Base    
      cattr_accessor :element_name    
      belongs_to :user, :track
      self.site = "#{self.site}/tracks/:track_id/"
      self.element_name = 'comment'   
      
      def initialize(options)   
        if not options[:track].nil?
          options[:track_id] = options[:track].id
          options.delete(:track)
        end
        super(options)
      end
      
    end   
        
  end
end
