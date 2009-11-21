require File.dirname(__FILE__) + '/spec_helper'

describe "Soundcloud::Models::Group" do 
  before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})

    @api_test_1 = @sc.User.find('api-test-1')    
    @api_test_2 = @sc.User.find('api-test-2')    
    @api_test_3 = @sc.User.find('api-test-3')
  end
  
  # static-test-group id = 2937
  # api_test_1 - creator
  # api_test_3 - member
  # api_test_2 track1 should be contributied
  
  it 'should find all (50) groups' do
    @sc.Group.find(:all)
  end
  
  it 'should get the fixture group' do
    group = @sc.Group.find(2937)
    group.name.should == "static-test-group"
  end
  
  
  describe 'users' do
    before do
      @group = @sc.Group.find(2937)
    end
    
    it 'should have the right creatotr api_test_1' do
      @group.creator.uri.should == @api_test_1.uri
    end
    
    it 'should have api_test_3 has a member' do
      @group.members.should include(@api_test_3)
    end
    
    it 'should have api_test_2 as a contributor' do
      @group.contributors.should include(@api_test_2)
    end
    
    it 'should have a contributed track' do
      @group.tracks.map(&:uri).should include('http://api.sandbox-soundcloud.com/tracks/875948')
    end
    
  end
  
end
