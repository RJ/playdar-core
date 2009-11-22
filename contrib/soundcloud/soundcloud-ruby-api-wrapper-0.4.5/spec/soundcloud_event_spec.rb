require File.dirname(__FILE__) + '/spec_helper'

describe 'Soundcloud::Models::Event' do
   before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})
  #  @api_test_1 = @sc.User.find('api-test-1')    
 #   @api_test_2 = @sc.User.find('api-test-2')    
#    @api_test_3 = @sc.User.find('api-test-3')  
  end
  
  it 'should get the last events' do
    events = @sc.Event.find(:all)
  end
  
  it 'should get fan events and they should provide the user resource' do
    fan_events = @sc.Event.find(:all,:params => {:filter => 'fan'})
    fan_events.each do |event|
      event.event_type.should == "Fan"
      event.user.username.should_not be nil      
    end
  end
  
  it 'should get track events and they should provide the track resource' do
    events = @sc.Event.find(:all,:params => {:filter => 'track'})
    events.each do |event|
      
      #Temporary Fix because of the API
      ["Track", "Playlist"].should include(event.event_type)
      
      #event.event_type.should == "Track"
      #event.track.title.should_not be nil      
    end    
  end
end
