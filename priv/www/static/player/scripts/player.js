//if ("WebSocket" in window) alert('yes'); else alert('no');

function myalert(json) { window.pl.addResult(json); }


Player = function(){}
Player.prototype = {
  isPlaying: false,
  currentTrack : {},
  
  go: function() {
    Playdar.MAX_CONCURRENT_RESOLUTIONS = 15;
    Playdar.USE_JSONP=false;
    Playdar.USE_STATUS_BAR=false;
    Playdar.USE_SCROBBLER = false;
    Playdar.MAX_POLLS = 20;
    Playdar.USE_COMET = true;
    
    Playdar.setupClient({
      onAuth: function () {
	  p.initialize();
      },
      onStatTimeout: function(){ alert('No playdar, FAIL. start playdar then refresh player'); },
      // Called in response to each poll with the results so far.
      onResults: function (response, lastPoll) {
	  p.playdarResult(response, lastPoll);
      }
    });
    MK.setWindowTitle("Playdar stat..");
    Playdar.client.go();
  },
  
  initialize: function() {
    MK.setWindowTitle("Env init..");
    this.playlists = [];
    var self = this;
    this.playButton = $('#play');
    this.playButton.click(function() {
      self.togglePlay();
    });
    
    sidebarWidth = 220;
        
    $("#sidebar").width(sidebarWidth);
    $("#main-container").css("left",sidebarWidth);

    this.volume = 75;//parseFloat($.cookie('volume'));
    if(!this.volume) {
      this.volume = 100; // default to max
    }

    // volume
    $("#volume").slider({
      value : this.volume,
      min : 0,
      max : 100,
      slide : function(e, ui) {
        self.volume = ui.value;
	MK.setVolume(self.volume);
      },
      change : function(e, ui) {
        // save the volume in a cookie
      }
    });

    $('#next')
      .click(function() {
	pl.playNext();
      })
      .dblclick(function() {
	pl.playAlternateOrNext();
      });

    $('#prev').click(function() {
      alert('prev');
    }); 
   
    // add playlist button
    $("#loop").click(function(ev) {
      var spiff = prompt("Enter XSPF URL","");
      loadSpiff(spiff);
      //alert('TODO, load ' + spiff);
    });
    
    // click behaviour for transport buttons
    $("#play,#prev,#next,#rand,#loop,#add-playlist").mousedown(function() {
      $(this).addClass("click");
    }).mouseup(function() {
      $(this).removeClass("click");
    });
    
    /*
    $('#loop').click(function(){
      p.play({'artist':'Mokele', 'track':'Hiding in your insides', 'url':'http://www.playdar.org/hiding.mp3'}, 'http://www.playdar.org/hiding.mp3');
    });
    */

    // toggle unplayable visibility
    $('#rand').click(function(){
      $('tr.unplayable').toggleClass('hidden');
    });
    
    this.playlists = {};
    
    // Wire up events from musickit container:
    
    // one of: playing stopped paused buffering loading
    MK.stateChange.connect(function(state){
      this.laststate = state;
      MK.setWindowTitle(this.laststate);
      MK.log('State: ' + state + ' isPlaying: ' + self.isPlaying);
      switch(this.laststate)
      {
	case 'stopped':
	  self.trackEnded();
	  //pl.playNext();
	  break;	  
	case 'playing':
	  self.trackStarted(self.currentTrack);
	  break;
	case 'error':
	case 'fatalerror':
	  if(self.isPlaying)
	  {
	    pl.playAlternateOrNext();
	  }
      }
    });
    
    MK.elapsed.connect(function(elapsed, remaining){
      MK.log('elapsed js ping, remaining: ' + remaining);
      var elapsedStr = Playdar.Util.mmss(elapsed);
      var durationStr;
      var widpc;
      if( remaining == -1 ) // unknown stream length
      {
	durationStr = Playdar.Util.mmss(self.currentTrack.duration) + ' ?';
	widpc = 100*elapsed/(self.currentTrack.duration+0.00001);
      } else {
	durationStr = Playdar.Util.mmss(remaining+elapsed);
	widpc = 100*elapsed/(elapsed+remaining+0.000001);
      }
      if(widpc>100) widpc=100;
      $('#position').html(elapsedStr);
      $('#duration').html(durationStr);
      $('#elapsed').css('width', widpc+'%');
    });
    
    MK.bufferPercent.connect(function(pc){
       //$('#buffered').css('width', pc +'%');
    });
    
    MK.volumeChanged.connect(function(v){
      $("#volume").slider('option', 'value', v);
    });
    
    MK.setWindowTitle("Ready.");
    
  },
  
  playdarResult: function(response, lastPoll){
    //pl.addResults(response.qid, response.results, lastPoll);
  },
  
  play: function(track, url) {
    MK.log('play: ' + JSON.stringify(track));
    MK.stop();
    var sid = track.sid;  
    var self = this;
    self.currentTrack = track;
    self.isPlaying = true;
    MK.play(url);
  },
  
  togglePlay : function() {
    MK.togglePause();
  },  
  
  stop: function() {
    MK.stop();  
    self.isPlaying = false;
    self.trackEnded();
  },
  
  trackEnded: function() {
    // TODO move to next track in list
    self.isPlaying = false;
    $("body").removeClass("playing");
    $('#np').fadeOut();
    $('#progress').fadeOut();
    $('#position').fadeOut();
    $('#duration').fadeOut();
  },
  
  trackStarted: function(track) {
    self.isPlaying = true;
    $("body").addClass("playing");
    //this.loading.css('width',"100%");
    $('#np').hide().html(track.artist + " - " + track.track).fadeIn();
    $('#position').hide().html("00:00").fadeIn();
    $('#duration').hide().html("00:00").fadeIn();
    $('#progress').fadeIn();
    $('#elapsed').css('width', '0%').fadeIn();
    //$('#loaded').css('width', '0%').fadeIn();
  },
  
  switchPlaylist: function(pl){
    self.currentPlaylist=pl;
    
  }
  
};


// Playdar



$(document).ready(function(){
  window.p = new Player();
  p.go();
  //loadSpiff("http://localhost:60210/static/toptracks.xspf");
loadSpiff("http://ws.audioscrobbler.com/1.0/tag/metal/toptracks.xspf");

  //req.open("GET", "file:///home/rj/src/playdar-core/priv/www/static/toptracks.xspf", false);
  //req.open("GET", "http://ws.audioscrobbler.com/1.0/tag/metal/toptracks.xspf", false);
  //alert(qid);
});

function loadSpiff(url)
{
  $('#lists table').remove();
  var req = new XMLHttpRequest();
  req.open("GET", url, false);
  req.send("");
  var doc = req.responseXML;//.documentElement;
  var jspf = XSPF.toJSPF(doc);
  window.pl = new Playlist();
  pl.init('0001', jspf.playlist.title);
  for(var i=0;i<jspf.playlist.track.length;i++)
  {
    var track  = jspf.playlist.track[i].title;
    var artist = jspf.playlist.track[i].creator;
    var dur    = parseInt(jspf.playlist.track[i].duration)/1000;
    pl.addTrack({ 'track':track,
          'artist': artist,
          'duration': dur});
  }
}




// Playlist class:
Playlist = function(){};
Playlist.prototype = {
  
  init: function(plid, name){
    var self=this;
    this.id = plid;
    this.name = name;
    this.playdarResults = {};
    this.lastQid = false;
    this.lastResultIndex = 0;
    $('#lists table').hide();
    $('#playlist-table')
      .clone()
      .attr('id',plid)
      .appendTo("#lists");
      
    this.dom = $("#lists > table:last");
    //this.dom = $('pl');
    this.list = $("tbody", this.dom);
  
    function withinHeaderDragArea(el,e) {
	var left = e.clientX-$(el).offset().left-($(el).width()+3);
	if(left > 0 && left < 4) {
	  return true;
	} else {
	  return false;
	}
    }
    $("th",this.dom)
      .mousemove(function(e) {
	if(withinHeaderDragArea(this,e)) {
	  $(this).css("cursor","col-resize");
	} else {
	  $(this).css("cursor","default");
	}
      })
      .mousedown(function(e) {
	var $col = $(this);
	var oldColWidth = $col.width();
	var colIdx = $(this).parents("thead").find("th").index(this) + 1;
	var rowWidth = $(this).parents("tr").width();
	var $row = $(this).parents("tr");
	var $rows = $("tr",self.list);

	if(withinHeaderDragArea(this,e)) {
	  $(document)
	    .mouseup(function() {
	      $(document).unbind("mousemove");
	    })
	    .mousemove(function(ev) {
	      var colWidth = ev.clientX - $col.offset().left;
	      if(colWidth >= 10)
	      {
		$col.width(colWidth);
		// resize all the cells in the same col
		$("td:nth-child(" + colIdx + ")", self.list).width(colWidth);
		$row.width(rowWidth+(colWidth-oldColWidth));
		$rows.width(rowWidth+(colWidth-oldColWidth));
	      }
	    });
	  }
      })
      .mouseup(function(e) {
	//var colIdx = $(this).parents("thead").find("th").index(this) + 1;
	//$.cookie('playlist_col_width_' + (colIdx-1),$(this).width());
      });
      
      $('<li id="'+this.id+'"><span></span>' + this.name + '</li>')
        .appendTo('#playlists');
  },
  
  play: function(qid){
    var res = this.playdarResults[qid];
    if(res.length<1)
    {
      alert('no results');
    }else{
      this.lastResultIndex = 0;
      var url = res[this.lastResultIndex].url || Playdar.client.get_stream_url(res[this.lastResultIndex].sid);
      $('#'+qid).addClass('playing');
      this.lastQid=qid;
      p.play(res[this.lastResultIndex], url);    
    }
  },
  
  playAlternateOrNext: function(){
    var qid = this.lastQid;
    var res = this.playdarResults[qid];
    // any more sources left to try?
    if(res.length > this.lastResultIndex+1)
    {
      //alert('trying alternate');
      this.lastResultIndex++;
      $('#'+qid+' td.alt').hide().text(res.length-(this.lastResultIndex)).fadeIn();
      this.setStatusCell(qid, res, true, this.lastResultIndex);
      var url = res[this.lastResultIndex].url || Playdar.client.get_stream_url(res[this.lastResultIndex].sid);
      p.play(res[this.lastResultIndex], url);
    }else{
      $('#'+qid).addClass('failed');
      //alert('playing next, no more alts');
      this.playNext();
    }
  },
  
  playNext: function(){
    this.lastResultIndex = 0;
    $('#'+this.lastQid).removeClass('playing');
    var nextTrs = $('#'+this.lastQid+' ~ tr.playable');
    if(nextTrs.length==0)
    {
      alert('fin');
    }
    else
    {
      var nextQ = nextTrs[0].id;
      //alert('next qid = ' +  nextQ);
      this.play(nextQ);
    }
    
  },
  
  addTrack: function(trk){
    if(!trk.qid) trk.qid=Playdar.Util.generate_uuid();
    $('#playlist-row')
      .clone()
      .attr('id', trk.qid)
      .dblclick(function() {
        //self.player.currentPlaylist = self;
        // find out at which position we are at in the playlist, and store that as the currentPos
        //self.currentPos = $(this).parents("tbody").find("tr").index(this);
        //$(this).addClass("selected");
	pl.play(trk.qid);
         //self.loadTrack(self.currentPos);
      })
      .find("td.track").text(trk.track).end()
      .find("td.artist").text(trk.artist).end()
      .find("td.duration").text(Playdar.Util.mmss(parseInt(trk.duration))).end()
      .find("td.status").text("Searching").end()
      .find("td.alt").text('').end()
      .appendTo(this.list);
    Playdar.client.resolve(trk.artist, trk.track, trk.album || "", trk.qid);
    return trk.qid;
  },
  
  setStatusCell: function(qid, results, lastPoll, idx){
    if(results.length < 1)
    {
      if(lastPoll) $('#'+qid+' td.status').html('Not found');
      else $('#'+qid+' td.status').html( $('#'+qid+' td.status').html()+'.');
    } 
    else 
    {
      var pc = Math.round( (results[idx].score || 0) * 100 );
      $('#'+qid+' td.status').html( pc + '%' + (results[idx].source?' - '+results[idx].source:'') );
    }
  },
  
  addResult: function(obj) {
    if(obj.method != 'results')
    {
      alert('Invalid result obj');
      return false;
    }
    
    var qid = obj.qid;
    if(!this.playdarResults[qid]) this.playdarResults[qid]=[];
    // insert but remain sorted in desc score order
    resultsloop: for(rid in obj.results)
    {
      var result = obj.results[rid];
      if(0 >= result.score) continue;
      for(var i=0; i<this.playdarResults[qid].length; i++)
      {
        if(result.score > this.playdarResults[qid][i]) 
        {
          this.playdarResults[qid].splice(i,0,result);
          break resultsloop;
        }
      }
      this.playdarResults[qid].push(result);
    }

    //alert(this.playdarResults[qid]);

    this.setStatusCell(qid, this.playdarResults[qid], false, 0);

    $('#'+qid+' td.status').click(function(){
      pl.play(qid);
    });

    if(this.playdarResults[qid].length)
    {
      if(this.playdarResults[qid][0].score >= 0.8)
      {
        $('#'+qid).addClass('playable').removeClass('unplayable');
      }
      $('#'+qid+' td.alt').html( this.playdarResults[qid].length );
    }
    else if(lastPoll && !$('#'+qid).hasClass('playable'))
    {
      $('#'+qid).addClass('unplayable');
    }
  },

  addResults: function(qid, results, lastPoll){
alert('addResults');
    this.playdarResults[qid]=results;
    this.setStatusCell(qid, results, lastPoll, 0);
    $('#'+qid+' td.status').click(function(){
      pl.play(qid);
    });
    if(results.length)
    {
      if(results[0].score >= 0.8)
      {
	$('#'+qid).addClass('playable').removeClass('unplayable');
      }
      $('#'+qid+' td.alt').html( results.length );
    }else if(lastPoll && !$('#'+qid).hasClass('playable')) $('#'+qid).addClass('unplayable');
  }
  
};    




