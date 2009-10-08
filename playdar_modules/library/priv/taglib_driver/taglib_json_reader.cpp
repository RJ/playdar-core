/*
    An erlang driver for taglib that outputs JSON.

    This process is passed filenames on stdin (from erlang)
    It uses taglib on the file, and outputs JSON containing the
    metadata, according to taglib. One line of JSON output per file.

*/
#include <taglib/fileref.h>
#include <taglib/tag.h>

#include <boost/algorithm/string.hpp>

#include <iostream>
#include <cstdio>
#include <sstream>

#include <netinet/in.h> // for htonl etc

using namespace std;

#ifdef WIN32
wstring fromUtf8(const string& i);
string toUtf8(const wstring& i);
#else
#define fromUtf8(s) (s)
#define toUtf8(s) (s)
#endif

string urlify(const string& p)
{
    // turn it into a url by prepending file://
    // because we pass all urls to curl:
    string urlpath("file://");
    if (p.at(0)=='/') // posix path starting with /
    {
        urlpath += p;
    }
    else if (p.at(1)==':') // windows style filepath
    {
        urlpath += "/" + p;
    }
    else urlpath = p;
    return urlpath;
}

string ext2mime(const string& ext)
{
    if(ext==".mp3") return "audio/mpeg";
    if(ext==".aac") return "audio/mp4";
    if(ext==".mp4") return "audio/mp4";
    if(ext==".m4a") return "audio/mp4";
    cerr << "Warning, unhandled file extension. Don't know mimetype for " << ext << endl;
     //generic:
    return "application/octet-stream";
}

// replace whitespace and other control codes with ' ' and replace multiple whitespace with single
string tidy(const string& s)
{
    string r;
    bool prevWasSpace = false;
    r.reserve(s.length());
    for (string::const_iterator i = s.begin(); i != s.end(); i++) {
        if (*i > 0 && *i <= ' ') {
            if (!prevWasSpace) {
                r += ' ';
                prevWasSpace = true;
            }
        } else if (*i == '"') {
            r += '\\';
            r += '"';
            prevWasSpace = false;
        } else {
            r += *i;
            prevWasSpace = false;
        }
    }
    return r;
}



string scan_file(const char* path)
{
    TagLib::FileRef f(path);
    if (!f.isNull() && f.tag()) {
        TagLib::Tag *tag = f.tag();
        int bitrate = 0;
        int duration = 0;
        if (f.audioProperties()) {
            TagLib::AudioProperties *properties = f.audioProperties();
            duration = properties->length();
            bitrate = properties->bitrate();
        }
        string artist = tag->artist().toCString(true);
        string album  = tag->album().toCString(true);
        string track  = tag->title().toCString(true);
        boost::trim(artist);
        boost::trim(album);
        boost::trim(track);
        if (artist.length()==0 || track.length()==0) {
            return "{\"error\" : \"no tags\"}\n";
        }
        string pathstr(path);
        string ext = pathstr.substr(pathstr.length()-4);
        string mimetype = ext2mime(boost::to_lower_copy(ext));
        // turn it into a url by prepending file://
        // because we pass all urls to curl:
        string urlpath = urlify( toUtf8(path) );

        ostringstream os;
        os      <<  "{  \"url\" : \"" << urlpath << "\","
                    "   \"mimetype\" : \"" << mimetype << "\","
                    "   \"artist\" : \"" << tidy(artist) << "\","
                    "   \"album\" : \"" << tidy(album) << "\","
                    "   \"track\" : \"" << tidy(track) << "\","
                    "   \"duration\" : " << duration << ","
                    "   \"bitrate\" : " << bitrate << ","
                    "   \"trackno\" : " << tag->track()

                <<  "}\n";
        return os.str();
    }
    return "{\"error\" : \"no tags\"}\n";
}

int readn(unsigned char *buf, int len)
{
    int i, got = 0;
    do 
    {
        if((i=read(0,buf+got, len-got))<=0) return i;
        got += i;
    } while(got<len);
    return len;
}

int writen(unsigned char *buf, int len)
{
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0) return i;
    wrote += i;
  } while (wrote<len);
  return len;
}


#ifdef WIN32
int wmain(int argc, wchar_t* argv[])
#else
int main(int argc, char* argv[])
#endif
{
    //scan_file( argv[1] );
    unsigned char buffer[256];
    unsigned int len0,len;
    while(1)
    {
        if(readn((unsigned char*)&len0,4)!=4) break;
        len = ntohl(len0);
        readn((unsigned char*)&buffer, len);
        buffer[len]='\0';
        string j = scan_file((const char*)&buffer);
        unsigned int l = htonl(j.length());
        writen((unsigned char*)&l,4);
        printf("%s", j.c_str());
        cout.flush();
    }

}

