require File.dirname(__FILE__) + '/spec_helper'

describe "Soundcloud::Models::User" do 
  before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})

    @api_test_1 = @sc.User.find('api-test-1')    
    @api_test_2 = @sc.User.find('api-test-2')    
    @api_test_3 = @sc.User.find('api-test-3')  
    # TODO do the  before(:something)
    begin
      #remove, just in case it's a contact already
      @api_test_3.remove_contact!
    rescue
      #ignore
    end
  end
  
  it 'should find a user with a specific name' do
    test_for = 'api-test-2'
    user = @sc.User.find(test_for)
    user.username.should == test_for
  end
  
  it 'should find all api-test users' do
    test_for = 'api-test'
    users = @sc.User.find(:all , :params => {:q => test_for})
    users.length.should be >= 3
  end
  
  it 'should check if a user has a contact' do 
    @api_test_2.has_contact?(@api_test_3).should be true
    @api_test_2.has_contact?(@api_test_3.id).should be true    
    @api_test_3.has_contact?(@api_test_1).should be false
  end
  
  it 'should add, check and remove a contact' do
    @api_test_3.is_contact?.should be false
    @api_test_3.add_contact!
    @api_test_3.is_contact?.should be true
    @api_test_3.remove_contact!    
  end

  it 'should check if a user has a favorite' do
    track1 = @sc.Track.find(:one, :from => '/users/api-test-2/tracks/track3-1')
    track2 = @sc.Track.find(:one, :from => '/users/api-test-2/tracks/track1-2')
    @api_test_2.has_favorite?(track1).should be true
    @api_test_2.has_favorite?(track1.id).should be true    
    @api_test_2.has_favorite?(track2).should be false
  end      
  
  it 'should find the logged in user' do 
    my_user = @sc.User.find_me
    my_user.username.should_not be nil
  end
  
  it 'should find some fans of a user' do 
    @api_test_2.fans.length.should be >= 1
  end
  
  it 'should find exactly one fan / two fans' do 
    @api_test_2.fans({:limit => 1}).length.should be == 1
    @api_test_2.fans({:limit => 2}).length.should be == 2
  end
  
end


