module Soundcloud
  module Models  
    # Look up the resource attributes and filtering usage here:
    #
    # SC API Attributes (as of 26/05/09):
    # ......
    # * users (array)
    # * moderators (array)
    # * members (array)
    # * contributors (array)
    # * tracks (array)
    
    class Group < Base
      has_many :users, :moderators, :members, :contributors, :tracks
      
      # NOT IMPLEMENTED ON THE API YET
      #cattr_accessor :data_attributes
      #self.data_attributes = ['artwork_data']      
      
      cattr_accessor :element_name
      self.element_name = 'group'   
    end    
  end
end
