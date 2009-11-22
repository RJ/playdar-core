module Soundcloud
  module Models
    class Base < OAuthActiveResource::Resource #:nodoc:
      def send_files(method, path, resource)
        params = {}
        self.attributes.reject { |k,v| data_attributes.include?(k)}.each { |k,v| 
          params["#{resource}[#{k}]"] = v
        }
        
        files = {}
        data_attributes.each do |attr|
          files["#{resource}[#{attr}]".to_sym] = self.attributes[attr] if self.attributes.has_key?(attr)
          self.attributes[attr] = nil
        end
        
        response = connection.handle_response(self.class.send_multipart_request(method,path,files,params))
       
        self.id = id_from_response(response)
        load_attributes_from_response(response)
      
      end
      
      # has_many_single_changeable and can_be_a_single_changeable is mostly used in combination with has_many.
      #
      # can_be_a_single_changeable expects to have a resource /me which is the logged-in user
      #
      # like in self.has_many you have a resource user with a sub resource friends,
      # but its not allowed to send a PUT /me/friends to update the list of friends
      # instead to add or remove a friend you have to send a GET/PUT/DELETE to /me/friends/{user_id}
      #
      # Example:
      #
      #   class User < Resource
      #     has_many :friends
      #     can_be_a_single_changeable :friend
      #     has_many_single_changeable :friends
      #   end
      #
      #   me = User.find(:one, :from => '/me')
      #   friend = me.friends.first
      #   stranger = User.find(235)
      # 
      #   friend.is_friend?
      #  => true
      #   stranger.is_friend?
      #  => false
      #
      #   strange.add_friend!
      #   stranger.is_friend?
      #  => true
      #
      #   stranger.remove_friend!
      #   stranger.is_friend?
      #  => false    
      #
      #   friend.has_friend?(stranger.id)
      #  => checks if stranger and friend are friend, returns true or false

      def self.can_be_a_single_changeable(*args)
        args.each do |k| 
          singular = k.to_s
          define_method("is_#{singular}?") do
            begin
              self.connection.get_without_decoding "/me/#{singular.pluralize}/#{self.id}"
              return true
            rescue ActiveResource::ResourceNotFound
              return false
            end
          end
          
          define_method("add_#{singular}!") do
            self.connection.put "/me/#{singular.pluralize}/#{self.id}"
          end                    

          define_method("remove_#{singular}!") do
            self.connection.delete "/me/#{singular.pluralize}/#{self.id}"
          end                
        end    
      end

      # see can_be_a_single_changeable        
      def self.has_many_single_changeable(*args)
        args.each do |k| 
          singular = k.to_s.singularize
          define_method("has_#{singular}?") do |object_or_id|
            if object_or_id.is_a? String or object_or_id.is_a? Integer          
              look_for_id = object_or_id
            else
              look_for_id = object_or_id.id
            end
              
            begin
              self.connection.get_without_decoding "/#{self.element_name.pluralize}/#{self.id}/#{singular.pluralize}/#{look_for_id}"
              return true
            rescue ActiveResource::ResourceNotFound
              return false
            end          
          end
        end    
      end    
    end
  end
end
