#!/usr/bin/python
import os, sys, urllib2, urllib, time
import simplejson as json
class AmieStreet:
  def __init__(self, gateway_url):
    self.gateway_url=gateway_url
  
  def __doRequest(self, service, method, params=None):
    param_string = ''
    if params!=None:
      param_string = urllib.urlencode(params)
    
    url = self.gateway_url+service+'/'+method+'.json?'+param_string
    c=urllib2.urlopen(url)
    contents = c.read()
    return json.read(contents)
  
  # Service: ArtistApi
  def ArtistApi_get(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'get', params)
  
  def ArtistApi_multiGet(self, artistIdString):
    params={'artistIdString':artistIdString}
    return self.__doRequest('ArtistApi', 'multiGet', params)
  
  def ArtistApi_info(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'info', params)
  
  def ArtistApi_songs(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'songs', params)
  
  def ArtistApi_topSongs(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'topSongs', params)
  
  def ArtistApi_albums(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'albums', params)
  
  def ArtistApi_topAlbums(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'topAlbums', params)
  
  def ArtistApi_similarArtists(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'similarArtists', params)
  
  def ArtistApi_labelSimilarArtists(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'labelSimilarArtists', params)
  
  def ArtistApi_genreSimilarArtists(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'genreSimilarArtists', params)
  
  def ArtistApi_recs(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'recs', params)
  
  def ArtistApi_bio(self, artist):
    params={'artist':artist}
    return self.__doRequest('ArtistApi', 'bio', params)
  
  # Service: AlbumApi
  def AlbumApi_get(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'get', params)
  
  def AlbumApi_multiGet(self, albumIdString):
    params={'albumIdString':albumIdString}
    return self.__doRequest('AlbumApi', 'multiGet', params)
  
  def AlbumApi_similar(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'similar', params)
  
  def AlbumApi_songs(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'songs', params)
  
  def AlbumApi_info(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'info', params)
  
  def AlbumApi_topSongs(self, album, limit):
    params={'album':album,'limit':limit}
    return self.__doRequest('AlbumApi', 'topSongs', params)
  
  def AlbumApi_recs(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'recs', params)
  
  def AlbumApi_topRecsWithReview(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'topRecsWithReview', params)
  
  def AlbumApi_editorNote(self, album):
    params={'album':album}
    return self.__doRequest('AlbumApi', 'editorNote', params)
  
  def AlbumApi_getUsersWhoPurchasedWithPrices(self, album, offset, limit):
    params={'album':album,'offset':offset,'limit':limit}
    return self.__doRequest('AlbumApi', 'getUsersWhoPurchasedWithPrices', params)
  
  # Service: SongApi
  def SongApi_getSongById(self, id):
    params={'id':id}
    return self.__doRequest('SongApi', 'getSongById', params)
  
  def SongApi_recordPostPurchasePlay(self, id):
    params={'id':id}
    return self.__doRequest('SongApi', 'recordPostPurchasePlay', params)
  
  def SongApi_getUsersWhoPurchasedWithPrices(self, song, offset, limit):
    params={'song':song,'offset':offset,'limit':limit}
    return self.__doRequest('SongApi', 'getUsersWhoPurchasedWithPrices', params)
  
  # Service: LibraryApi
  def LibraryApi_numSongs(self, user):
    params={'user':user}
    return self.__doRequest('LibraryApi', 'numSongs', params)
  
  def LibraryApi_songs(self, user, page, limit):
    params={'user':user,'page':page,'limit':limit}
    return self.__doRequest('LibraryApi', 'songs', params)
  
  def LibraryApi_artists(self, user):
    params={'user':user}
    return self.__doRequest('LibraryApi', 'artists', params)
  
  def LibraryApi_albums(self, user):
    params={'user':user}
    return self.__doRequest('LibraryApi', 'albums', params)
  
  def LibraryApi_genres(self, user):
    params={'user':user}
    return self.__doRequest('LibraryApi', 'genres', params)
  
  def LibraryApi_banSong(self, fingerprint, songId):
    params={'fingerprint':fingerprint,'songId':songId}
    return self.__doRequest('LibraryApi', 'banSong', params)
  
  def LibraryApi_banSongList(self, fingerprint, songIdList):
    params={'fingerprint':fingerprint,'songIdList':songIdList}
    return self.__doRequest('LibraryApi', 'banSongList', params)
  
  def LibraryApi_unbanSong(self, fingerprint, songId):
    params={'fingerprint':fingerprint,'songId':songId}
    return self.__doRequest('LibraryApi', 'unbanSong', params)
  
  def LibraryApi_unbanSongList(self, fingerprint, songIdList):
    params={'fingerprint':fingerprint,'songIdList':songIdList}
    return self.__doRequest('LibraryApi', 'unbanSongList', params)
  
  def LibraryApi_bannedSongs(self, user):
    params={'user':user}
    return self.__doRequest('LibraryApi', 'bannedSongs', params)
  
  def LibraryApi_refreshLibrary(self, user, songIds):
    params={'user':user,'songIds':songIds}
    return self.__doRequest('LibraryApi', 'refreshLibrary', params)
  
  # Service: PlayerApi
  def PlayerApi_scrobble(self, fingerprint, songId, status, instance):
    params={'fingerprint':fingerprint,'songId':songId,'status':status,'instance':instance}
    return self.__doRequest('PlayerApi', 'scrobble', params)
  
  # Service: AuthApi
  def AuthApi_login(self, username, password):
    params={'username':username,'password':password}
    return self.__doRequest('AuthApi', 'login', params)
  
  def AuthApi_logout(self, fingerprint):
    params={'fingerprint':fingerprint}
    return self.__doRequest('AuthApi', 'logout', params)
  
  def AuthApi_getFingerprint(self):
    return self.__doRequest('AuthApi', 'getFingerprint')
  
  def AuthApi_isFingerprintValid(self, fingerprint):
    params={'fingerprint':fingerprint}
    return self.__doRequest('AuthApi', 'isFingerprintValid', params)
  
  def AuthApi_getActor(self, fingerprint):
    params={'fingerprint':fingerprint}
    return self.__doRequest('AuthApi', 'getActor', params)
  
  def AuthApi_requestOfflineToken(self, username, password, appName):
    params={'username':username,'password':password,'appName':appName}
    return self.__doRequest('AuthApi', 'requestOfflineToken', params)
  
  def AuthApi_useOfflineToken(self, token, appName):
    params={'token':token,'appName':appName}
    return self.__doRequest('AuthApi', 'useOfflineToken', params)
  
  # Service: FreebiesApi
  def FreebiesApi_getPlaylist(self, genreGroupId, excludeSongIds):
    params={'genreGroupId':genreGroupId,'excludeSongIds':excludeSongIds}
    return self.__doRequest('FreebiesApi', 'getPlaylist', params)
  
  def FreebiesApi_getPlaylistSongIds(self, genreGroupId, excludeSongIds):
    params={'genreGroupId':genreGroupId,'excludeSongIds':excludeSongIds}
    return self.__doRequest('FreebiesApi', 'getPlaylistSongIds', params)
  
  def FreebiesApi_getAllGenresList(self):
    return self.__doRequest('FreebiesApi', 'getAllGenresList')
  
  def FreebiesApi_getAllGenreGroups(self):
    return self.__doRequest('FreebiesApi', 'getAllGenreGroups')
  
  def FreebiesApi_freeCountInGroup(self, genreGroupId):
    params={'genreGroupId':genreGroupId}
    return self.__doRequest('FreebiesApi', 'freeCountInGroup', params)
  
  # Service: BrowseApi
  def BrowseApi_newReleases(self, offset, limit, genreGroupId):
    params={'offset':offset,'limit':limit,'genreGroupId':genreGroupId}
    return self.__doRequest('BrowseApi', 'newReleases', params)
  
  def BrowseApi_popular(self, offset, limit, genreGroupId):
    params={'offset':offset,'limit':limit,'genreGroupId':genreGroupId}
    return self.__doRequest('BrowseApi', 'popular', params)
  
  def BrowseApi_top25Albums(self, genreGroupId):
    params={'genreGroupId':genreGroupId}
    return self.__doRequest('BrowseApi', 'top25Albums', params)
  
  def BrowseApi_top25Songs(self, genreGroupId):
    params={'genreGroupId':genreGroupId}
    return self.__doRequest('BrowseApi', 'top25Songs', params)
  
  def BrowseApi_artists(self, genreIds, sort, page, perPage):
    params={'genreIds':genreIds,'sort':sort,'page':page,'perPage':perPage}
    return self.__doRequest('BrowseApi', 'artists', params)
  
  def BrowseApi_albums(self, genreIds, sort, priceFilter, page, perPage):
    params={'genreIds':genreIds,'sort':sort,'priceFilter':priceFilter,'page':page,'perPage':perPage}
    return self.__doRequest('BrowseApi', 'albums', params)
  
  def BrowseApi_songs(self, genreIds, sort, priceFilter, page, perPage):
    params={'genreIds':genreIds,'sort':sort,'priceFilter':priceFilter,'page':page,'perPage':perPage}
    return self.__doRequest('BrowseApi', 'songs', params)
  
  def BrowseApi_genreGroupTree(self):
    return self.__doRequest('BrowseApi', 'genreGroupTree')
  
  # Service: SearchApi
  def SearchApi_artists(self, query):
    params={'query':query}
    return self.__doRequest('SearchApi', 'artists', params)
  
  def SearchApi_albums(self, query):
    params={'query':query}
    return self.__doRequest('SearchApi', 'albums', params)
  
  def SearchApi_songs(self, query):
    params={'query':query}
    return self.__doRequest('SearchApi', 'songs', params)
  
  def SearchApi_find(self, artists, songs):
    params={'artists':artists,'songs':songs}
    return self.__doRequest('SearchApi', 'find', params)
  
  # Service: PricingApi
  def PricingApi_song(self, id):
    params={'id':id}
    return self.__doRequest('PricingApi', 'song', params)
  
  # Service: UserApi
  def UserApi_playlists(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'playlists', params)
  
  def UserApi_get(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'get', params)
  
  def UserApi_multiGet(self, userIdList):
    params={'userIdList':userIdList}
    return self.__doRequest('UserApi', 'multiGet', params)
  
  def UserApi_friends(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'friends', params)
  
  def UserApi_create(self, email, password, username):
    params={'email':email,'password':password,'username':username}
    return self.__doRequest('UserApi', 'create', params)
  
  def UserApi_savedItemCount(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'savedItemCount', params)
  
  def UserApi_savedAlbums(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'savedAlbums', params)
  
  def UserApi_savedSongs(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'savedSongs', params)
  
  def UserApi_recs(self, user, limit, offset):
    params={'user':user,'limit':limit,'offset':offset}
    return self.__doRequest('UserApi', 'recs', params)
  
  def UserApi_playHistory(self, user, limit, offset):
    params={'user':user,'limit':limit,'offset':offset}
    return self.__doRequest('UserApi', 'playHistory', params)
  
  def UserApi_save(self, fingerprint, type, id):
    params={'fingerprint':fingerprint,'type':type,'id':id}
    return self.__doRequest('UserApi', 'save', params)
  
  def UserApi_fannedArtists(self, user):
    params={'user':user}
    return self.__doRequest('UserApi', 'fannedArtists', params)
  
  # Service: PlaylistApi
  def PlaylistApi_get(self, playlistId):
    params={'playlistId':playlistId}
    return self.__doRequest('PlaylistApi', 'get', params)
  
  def PlaylistApi_multiGet(self, playlistIdList):
    params={'playlistIdList':playlistIdList}
    return self.__doRequest('PlaylistApi', 'multiGet', params)
  
  def PlaylistApi_create(self, fingerprint, title, description):
    params={'fingerprint':fingerprint,'title':title,'description':description}
    return self.__doRequest('PlaylistApi', 'create', params)
  
  def PlaylistApi_copy(self, fingerprint, playlistId):
    params={'fingerprint':fingerprint,'playlistId':playlistId}
    return self.__doRequest('PlaylistApi', 'copy', params)
  
  def PlaylistApi_info(self, playlistId):
    params={'playlistId':playlistId}
    return self.__doRequest('PlaylistApi', 'info', params)
  
  def PlaylistApi_addCollaborator(self, fingerprint, playlistId, collaboratorId):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'collaboratorId':collaboratorId}
    return self.__doRequest('PlaylistApi', 'addCollaborator', params)
  
  def PlaylistApi_removeCollaborator(self, fingerprint, playlistId, collaboratorId):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'collaboratorId':collaboratorId}
    return self.__doRequest('PlaylistApi', 'removeCollaborator', params)
  
  def PlaylistApi_addSong(self, fingerprint, playlistId, songId, position):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'songId':songId,'position':position}
    return self.__doRequest('PlaylistApi', 'addSong', params)
  
  def PlaylistApi_addSongList(self, fingerprint, playlistId, songIdList, position):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'songIdList':songIdList,'position':position}
    return self.__doRequest('PlaylistApi', 'addSongList', params)
  
  def PlaylistApi_removeSong(self, fingerprint, playlistId, songId):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'songId':songId}
    return self.__doRequest('PlaylistApi', 'removeSong', params)
  
  def PlaylistApi_removeSongList(self, fingerprint, playlistId, songIdList):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'songIdList':songIdList}
    return self.__doRequest('PlaylistApi', 'removeSongList', params)
  
  def PlaylistApi_updateSequence(self, fingerprint, playlistId, songIdList):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'songIdList':songIdList}
    return self.__doRequest('PlaylistApi', 'updateSequence', params)
  
  def PlaylistApi_updateTitle(self, fingerprint, playlistId, title):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'title':title}
    return self.__doRequest('PlaylistApi', 'updateTitle', params)
  
  def PlaylistApi_updateDescription(self, fingerprint, playlistId, description):
    params={'fingerprint':fingerprint,'playlistId':playlistId,'description':description}
    return self.__doRequest('PlaylistApi', 'updateDescription', params)
  
  def PlaylistApi_deletePlaylist(self, fingerprint, playlistId):
    params={'fingerprint':fingerprint,'playlistId':playlistId}
    return self.__doRequest('PlaylistApi', 'deletePlaylist', params)
  
  # Service: ShrekApi
  def ShrekApi_autocomplete(self, query):
    params={'query':query}
    return self.__doRequest('ShrekApi', 'autocomplete', params)
  
  def ShrekApi_recommend(self, pids):
    params={'pids':pids}
    return self.__doRequest('ShrekApi', 'recommend', params)
  
  def ShrekApi_playlist(self, artistIds, excludeSongIds):
    params={'artistIds':artistIds,'excludeSongIds':excludeSongIds}
    return self.__doRequest('ShrekApi', 'playlist', params)
  
  def ShrekApi_stationToPids(self, station):
    params={'station':station}
    return self.__doRequest('ShrekApi', 'stationToPids', params)
  
  # Service: AutocompleteApi
  def AutocompleteApi_artists(self, query):
    params={'query':query}
    return self.__doRequest('AutocompleteApi', 'artists', params)
  
  def AutocompleteApi_albums(self, query):
    params={'query':query}
    return self.__doRequest('AutocompleteApi', 'albums', params)
  
  def AutocompleteApi_songs(self, query):
    params={'query':query}
    return self.__doRequest('AutocompleteApi', 'songs', params)
  
  # Service: RecApi
  def RecApi_latest(self, offset, limit):
    params={'offset':offset,'limit':limit}
    return self.__doRequest('RecApi', 'latest', params)
  
  def RecApi_score(self, fingerprint, recId, score):
    params={'fingerprint':fingerprint,'recId':recId,'score':score}
    return self.__doRequest('RecApi', 'score', params)
  
  def RecApi_latestForTraining(self, fingerprint):
    params={'fingerprint':fingerprint}
    return self.__doRequest('RecApi', 'latestForTraining', params)
  
  # Service: RecommendationsApi
  def RecommendationsApi_isNew(self):
    return self.__doRequest('RecommendationsApi', 'isNew')
  
  def RecommendationsApi_refresh(self):
    return self.__doRequest('RecommendationsApi', 'refresh')
  
  def RecommendationsApi_addSeedArtist(self, name, source):
    params={'name':name,'source':source}
    return self.__doRequest('RecommendationsApi', 'addSeedArtist', params)
  
  def RecommendationsApi_removeSeedArtist(self, name):
    params={'name':name}
    return self.__doRequest('RecommendationsApi', 'removeSeedArtist', params)
  
  def RecommendationsApi_listSeedArtists(self):
    return self.__doRequest('RecommendationsApi', 'listSeedArtists')
  
  def RecommendationsApi_recommend(self, offset, count, genreGroup, fingerprint):
    params={'offset':offset,'count':count,'genreGroup':genreGroup,'fingerprint':fingerprint}
    return self.__doRequest('RecommendationsApi', 'recommend', params)
  
  # Service: SalesReportApi
  def SalesReportApi_getBasicSalesReport(self, quarter, year):
    params={'quarter':quarter,'year':year}
    return self.__doRequest('SalesReportApi', 'getBasicSalesReport', params)
  
  # Service: PandoraApi
  def PandoraApi_stations(self, user):
    params={'user':user}
    return self.__doRequest('PandoraApi', 'stations', params)
  
  def PandoraApi_bookmarkedSongs(self, user):
    params={'user':user}
    return self.__doRequest('PandoraApi', 'bookmarkedSongs', params)
  
  def PandoraApi_bookmarkedArtists(self, user):
    params={'user':user}
    return self.__doRequest('PandoraApi', 'bookmarkedArtists', params)
  
  # Service: LastfmApi
  def LastfmApi_userTopArtists(self, user, period):
    params={'user':user,'period':period}
    return self.__doRequest('LastfmApi', 'userTopArtists', params)
  
  # Service: HypemachineApi
  def HypemachineApi_lovedTracks(self, username, page):
    params={'username':username,'page':page}
    return self.__doRequest('HypemachineApi', 'lovedTracks', params)
  
  def HypemachineApi_listenHistory(self, username, page):
    params={'username':username,'page':page}
    return self.__doRequest('HypemachineApi', 'listenHistory', params)
  
  # Service: SongzaApi
  def SongzaApi_playlist(self, username):
    params={'username':username}
    return self.__doRequest('SongzaApi', 'playlist', params)
  
  # Service: TumblrApi
  def TumblrApi_newPost(self, email, password, title, body, tags, group):
    params={'email':email,'password':password,'title':title,'body':body,'tags':tags,'group':group}
    return self.__doRequest('TumblrApi', 'newPost', params)
  
  # Service: NewsletterApi
  def NewsletterApi_get(self, id):
    params={'id':id}
    return self.__doRequest('NewsletterApi', 'get', params)
  
  def NewsletterApi_latest(self, limit, offset):
    params={'limit':limit,'offset':offset}
    return self.__doRequest('NewsletterApi', 'latest', params)
  
  def NewsletterApi_items(self, id):
    params={'id':id}
    return self.__doRequest('NewsletterApi', 'items', params)
  
# End rest py client