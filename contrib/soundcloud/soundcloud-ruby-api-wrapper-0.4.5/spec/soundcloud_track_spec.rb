require File.dirname(__FILE__) + '/spec_helper'

describe "Soundcloud::Models::Track" do 
  before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})

    @api_test_1 = @sc.User.find('api-test-1')    
    @api_test_2 = @sc.User.find('api-test-2')    
    @api_test_3 = @sc.User.find('api-test-3')  
   
    test_tracks = @api_test_2.tracks
    @test_track_1 = test_tracks.first
  
    begin
      track = @sc.Track.find('static-test-track')
      track.permissions.delete( @api_test_3 )
      track.permissions << @api_test_2
      track.permissions.save
    rescue
    end
    begin
      @test_track_1.remove_favorite!    
    rescue
    end

  end
  
  
  it "should be able to create a new track and default to sharing=private" do
    test_track_file = File.new( File.dirname(__FILE__) + '/fixtures/test_track.mp3')
    track = @sc.Track.create({:title => "test", :asset_data => test_track_file})  
    track.sharing.should == 'private'
    track.permalink.should_not == nil
    track.destroy
  end
  
  it "should be able to create a new track with artwork" do
    test_track_file = File.new( File.dirname(__FILE__) + '/fixtures/test_track.mp3')
    test_artwork_file = File.new( File.dirname(__FILE__) + '/fixtures/test_artwork.gif')
    track = @sc.Track.create({:title => "test", :asset_data => test_track_file, :artwork_data => test_artwork_file})  
    
    track.artwork_url.should_not == nil
    track.destroy
  end
  
  it "should be able to create a new public track and sharing should stay public" do
    test_track_file = File.new( File.dirname(__FILE__) + '/fixtures/test_track.mp3')
    track = @sc.Track.create({:title => "test", :sharing => 'public', :asset_data => test_track_file})
    track.sharing.should == 'public'
    track.permalink.should_not == nil
    track.destroy
  end
  
  it "should be able to update a track artwork" do
    test_artwork_file = File.new( File.dirname(__FILE__) + '/fixtures/test_artwork.gif')
    track = @sc.Track.find('static-test-track')   
    old_artwork = track.artwork_url
    track.artwork_data = test_artwork_file
    
    track.save
    track.artwork_url.should_not == old_artwork
  end
  
  
  it 'should be able to create a new track and remove it' do
    test_track_file = File.new(File.dirname(__FILE__) + '/fixtures/test_track.mp3')

    track = @sc.Track.new
    track.title = 'API Test 1'
    track.sharing = 'private' 
    track.asset_data = test_track_file
    track.save

    track.destroy
        
    lambda { track.reload }.should raise_error ActiveResource::ResourceNotFound
  end
  
  it 'should be able to update an attribute' do
    track = @sc.Track.find('static-test-track')   
    time = Time.now
    track.title = "T #{time}"
    track.save
    track.title = 'something else'
    
    track.reload

    track.title.should == "T #{time}"
  end
  
  it 'should be able to update tag_list directly' do
    track = @sc.Track.find('static-test-track')
    time = Time.now
    track.tag_list = "T#{time} B#{time}"
    track.save
    track.tag_list = 'something else'

    track.reload

    track.tag_list.should == "T#{time} B#{time}" 
  end
  
 # it 'should be able to read the tags array' do
 #   track = @sc.Track.new
 #   
 #   track.tag_list = "bla blub"
 #   
 #   track.tags.include?('bla').should == true
 #   track.tags.include?('blub').should == true
 # end
 # 
 # 
 # it 'should be able to write to the tags array' do
 #   track = @sc.Track.new
 #   
 #   track.tags << 'bla'
 #   track.tags << 'blub'
 #   
 #   track.tag_list.should == "bla blub"
 # end
  
  #it 'should be able to update the tags array' do 
  #  track = @sc.Track.find('static-test-track')
  #  time = Time.now
  #  track.tags << "'bl ub'"
  #  track.save
  #  track.tags.include?("'bl ub'").should == true
  #  
  #  track.tags.delete('')
  #  
  #  #reload
  #  #tag_list.should_contain
  #  #tags.include? 'bla
  #end
  
  it 'should be able to add a user to permissions of a track and delete it again' do
    track = @sc.Track.find(:one, :from => '/users/api-test-1/tracks/static-test-track')   
    
    old_count = track.permissions.length

    track.permissions << @api_test_3    
    track.permissions.save
    
    track.permissions.length.should be(old_count+1)    
    
    track.permissions.delete( @api_test_3 )
    track.permissions.save
    
    track.permissions.length.should be(old_count)  
  end
  
  it 'should add, check and remove a favorite to "me"' do
    @test_track_1.is_favorite?.should be false
    @test_track_1.add_favorite!
    @test_track_1.is_favorite?.should be true
    @test_track_1.remove_favorite!
    @test_track_1.is_favorite?.should be false
  end  
  
  it 'should be able to download a private track' do
    track = @sc.Track.find(:one, :from => '/users/api-test-2/tracks/track3-1')
    track.download_url
  end
  
  it 'should be able to download a public track (unauthenticated)' do 
    usc = Soundcloud.register({:site => soundcloud_site})
    track = usc.Track.find(:one, :from => '/users/api-test-2/tracks/track1-2')
    track.download_url
  end
  
  it 'should find tracks with a bpm <= 90' do
    slow_tracks  = @sc.Track.find(:all, :params => { "bpm[to]" => "90"} )
    slow_tracks.each { |track| track.bpm.should be <= 90.0 }
  end
  
  it 'should belong to a user' do
    # check against online attribute, to make sure the complete user is loaded, not the nested user 
    @test_track_1.user.online.should_not be nil
  end

  it 'should resolve a track' do
    @sc.resolve(@test_track_1.permalink_url).should == @test_track_1
  end
end
