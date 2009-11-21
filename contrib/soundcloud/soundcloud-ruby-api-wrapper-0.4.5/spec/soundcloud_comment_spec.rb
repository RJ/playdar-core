require File.dirname(__FILE__) + '/spec_helper'

describe 'Soundcloud::Models::Comment' do
   before(:all) do
    @sc = Soundcloud.register({:access_token=> valid_oauth_access_token, :site => soundcloud_site})
#    @api_test_1 = @sc.User.find('api-test-1')    
#    @api_test_2 = @sc.User.find('api-test-2')    
#    @api_test_3 = @sc.User.find('api-test-3')  
    @track = @sc.Track.find('static-test-track')
  end
  
  it 'should be able to create and delete a new comment for a track' do

    old_count = @track.comments.length  
    comment = @sc.Comment.create({:track_id => @track.id, :body => "new API Test comment"})
    @track.comments.reload.length.should be old_count + 1
    
    comment.destroy
    
    @track.comments.reload.length.should be old_count
  end
  
  it 'should belong to a track and a user' do
    comment =  @track.comments.first
    
    comment.user.id.should_not be nil
    comment.track.id.should_not be nil
  end
  
  it 'should create a new comment and associate the track_id' do
    comment = @sc.Comment.new({:track => @track})
    comment.track_id.should be @track.id
  end
  
end
