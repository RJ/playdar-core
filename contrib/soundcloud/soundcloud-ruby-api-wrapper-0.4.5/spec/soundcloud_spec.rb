require File.dirname(__FILE__) + '/spec_helper'

# this spec tests against api.sandbox-soundcloud.com
# it makes the follwing assumptions
#
# the users api-test-1, api-test-2, api-test-3 exists
#
# the logged in user is api-test-1 
#
# api-test-1 follows api-test-2 but NOT api-test-3 (??)
# api-test-2 follows api-test-1 and api-test-3
# api-test-3 follows api-test-2 but NOT api-test-1
# 
# api-test-2 has 3 Tracks [Track1 (public,downloadable),Track2,Track3(private,downloadable)]
# api-test-2 has favorites: Track1 but NOT Track2
# api-test-2 has 'static-test-playlist' with Track1, 2 and 3
#
# api-test-1 has Track "static-test-track" and user api-test-3 has not permissions for it
# api-test-1 has a playlist "my-static-playlist" with at least 2 tracks



describe "Soundcloud" do  
  before(:all) do
  end
  
  it 'should create an oauth consumer' do
    Soundcloud.consumer('consumer_token','consumer_secret').should be_an_instance_of OAuth::Consumer
  end
  
  it 'should register a client without an oauth token' do
    sc = Soundcloud.register({:site => soundcloud_site})
    sc.to_s.should match(/Soundcloud::.+/)
    lambda{ sc.User.find(:one, :from => "/me")}.should raise_error ActiveResource::UnauthorizedAccess
  end
  
  it 'should register a client with an oauth token' do
    sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})
    sc.to_s.should match(/Soundcloud::.+/)
    lambda{ sc.User.find(:one, :from => "/me")}.should_not raise_error ActiveResource::UnauthorizedAccess
  end    
end
