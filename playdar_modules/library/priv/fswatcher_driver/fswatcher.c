#if __APPLE__

#include <CoreServices/CoreServices.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/dir.h>

static time_t global_last_mtime;

static void callback(ConstFSEventStreamRef stream,
                     void *userinfo,
                     size_t const N,
                     void *paths,
                     const FSEventStreamEventFlags flags[],
                     const FSEventStreamEventId ids[])
{
    time_t last_mtime = global_last_mtime;
    size_t i;
    DIR* d;
    struct dirent *dp;

    for (i = 0; i < N; i++) {
        const char* dname = ((char**)paths)[i];

        if ((d = opendir(dname)) == NULL) {
            perror(dname);
            continue;
        }
        while ((dp = readdir(d)) != NULL) {
            switch (dp->d_type) {
                case DT_REG: case DT_LNK: break;
                default: continue;
            }

            char path[dp->d_namlen + strlen(dname) + 1];
            strcpy(path, dname);
            strcat(path, dp->d_name);

            struct stat sb;
            if (lstat(path, &sb) < 0 && errno == EACCES)
                perror(path);
            else if (sb.st_mtime >= last_mtime) {
                puts(path);
                if (sb.st_mtime > global_last_mtime)
                    global_last_mtime = sb.st_mtime;
            }
        }
        closedir(d);
    }
    fflush(stdout);
}

static inline CFArrayRef paths(const int N, char** in)
{
    CFStringRef out[N];
    int x;
    for (x = 0; x < N; ++x)
        out[x] = CFStringCreateWithCString(kCFAllocatorDefault, in[x], kCFStringEncodingUTF8);
    return CFArrayCreate(NULL, (const void **)out, N, NULL);
}

int main(int argc, char** argv)
{
    global_last_mtime = time(NULL);

    FSEventStreamRef stream = FSEventStreamCreate(
            NULL,
            &callback,
            NULL,
            paths(--argc, ++argv),
            kFSEventStreamEventIdSinceNow, // TODO previous event ID
            5.0, // seconds after an event before calling our callback
            kFSEventStreamCreateFlagNone);
    FSEventStreamScheduleWithRunLoop(stream, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    FSEventStreamStart(stream);

    CFRunLoopRun();
}

#elif WIN32

int wmain(int argc, wchar_t* argv[])
{}

#endif
