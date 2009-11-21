require File.dirname(__FILE__) + '/spec_helper'

describe 'Soundcloud::Models::Playlist' do
   before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})
    @api_test_1 = @sc.User.find('api-test-1')   
    @api_test_2 = @sc.User.find('api-test-2')   
    @api_test_3 = @sc.User.find('api-test-3')
    
  end  
  
  it 'should be able to create and deleta a new playlist' do
    # TODO is not implemented in the soundcloud api
    #    pl = @sc.Playlist.new
    #   pl.title = 'Static Test Playlist'
    #     pl.tracks << @sc.Track.find('static-test-track')    
    
    #   p pl.to_xml
    #   pl.save
  end
  
  it 'should be able to find an existing playlist' do 
    pl = @sc.Playlist.find('static-test-playlist')
    pl.tracks.length.should be >= 3    
  end
  
  it 'should be able to delete tracks of a playlist and put them back' do 
    pl = @sc.Playlist.find('my-static-playlist')    
    old_count = pl.tracks.length
    deleted_track = pl.tracks.first
    pl.tracks.delete_at 0
    pl.save
    pl.tracks.length.should be == old_count -1
    
    pl.tracks << deleted_track
    pl.save
    pl.tracks.length.should be == old_count
  end
  
  it 'should belong to a user' do
    pl = @sc.Playlist.find('static-test-playlist')
    # check against online attribute, to make sure the complete user is loaded, not the nested user 
    pl.user.online.should_not be nil
  end
  
  
  it "should be able to update a playlist artwork" do
    test_artwork_file = File.new( File.dirname(__FILE__) + '/fixtures/test_artwork.gif')
    playlist = @sc.Playlist.find('my-static-playlist')
    old_artwork = playlist.artwork_url
    playlist.artwork_data = test_artwork_file
    
    playlist.save
    playlist.artwork_url.should_not == old_artwork
  end
  
  it "should be able to create a new playlist with artwork" do
    #track = @sc.Track.find('static-test-track')   
    test_artwork_file = File.new( File.dirname(__FILE__) + '/fixtures/test_artwork.gif')
        
    playlist = @sc.Playlist.create({:title => 'test', :artwork_data => test_artwork_file})
    playlist.artwork_url.should_not == nil
    #playlist.destroy
  end
end
