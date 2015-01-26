#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

const char * indent = "    ";

int printUsage()
{
    printf("Usage: directory_traverse_depth_first <dir>\n\n");

    return EINVAL;
}

int dirOpenError(const char * dir)
{
    printf("Error opening %s\n", dir);

    return errno;
}

int statError(const char * dir)
{
    printf("Error stat'ing %s\n", dir);

    return errno;
}

void printIndent(int depth)
{
    int i = 0;
    while (i++ < depth) {
        printf(indent);
    }
}

int dft(int depth, const char * dir, const char * name)
{
    int dirLen = strlen(dir), nameLen = strlen(name);
    int dir2Len = dirLen + 1 + nameLen + 1;
    char dir2[dir2Len];
    snprintf(dir2, dir2Len, "%s/%s", dir, name);

    struct stat s;
    int statRet = stat(dir2, &s);

    if (0 == statRet) {
        if (S_ISDIR(s.st_mode)) {
            DIR * dp = opendir(dir2);
            struct dirent *d;

            if (NULL == dp) {
                return dirOpenError(dir2);
            }

            printIndent(depth);
            printf("%s\n", name);
            while (NULL != (d = readdir(dp))) {
                if (0 != d->d_ino) {
                    if (0 == strcmp(".", d->d_name) || 0 == strcmp("..", d->d_name)) {
                        continue;
                    } else {
                        dft(depth + 1, dir2, d->d_name);
                    }
                }
            }

            closedir(dp);
        } else if (S_ISREG(s.st_mode)) {
            printIndent(depth);
            printf("%s\n", name);
        }
    } else if (-1 == statRet) {
        return statError(dir2);
    }

    return 0;
}

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        return printUsage();
    }

    return dft(0, ".", argv[1]);
}
