#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

// Since we can only use `close`, `lseek`, `open`, and `read`,
// we have to get tricky to keep this thing understandable.

const int BUF_SIZE = 4096;

void printUsage()
{
    printf("Usage: odds-then-evens <file>\n\n");
    printf("Prints the odd lines in a file followed by the even lines.\n");
}

int printLine(char str[], int const start)
{
    int i = start;
    for (; i < BUF_SIZE && '\n' != str[i]; ++i) {
        printf("%c", str[i]);
    }
    printf("\n");
    return i + 1;
}

int nextLine(char str[], int const start)
{
    int i = start;
    for (; i < BUF_SIZE && '\n' != str[i]; ++i) {}

    return i + 1;
}

int printEveryOther(int fd)
{
    char str[BUF_SIZE];
    ssize_t pos = read(fd, str, BUF_SIZE);
    int curPos = 0;
    printf("read: %d bytes\n", (int)pos);
    printf("str is: \n%s\n", str);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    curPos = printLine(str, curPos);
    curPos = nextLine(str, curPos);
    // do {
    //     if (-1 == pos) {
    //         printf("Error reading file");
    //         return errno;
    //     } else {
    //         // printLine(str);
    //         // nextLine(str);
    //     }
    //     curPos = lseek(fd, 0, SEEK_CUR);
    //     if (curPos >= BUF_SIZE) {
    //         lseek(fd, 0, SEEK_SET);
    //         pos = read(fd, str, BUF_SIZE);
    //     }
    // } while (pos != 0);
    return 0;
    // fseek(fd, -1, SEEK_CUR);
    // while (EOF != c) {
    //     printLine(fd);
    //     nextLine(fd);
    //     c = fgetc(fd);
    //     fseek(fd, -1, SEEK_CUR);
    // }
}

int oddsThenEvens(char const * path)
{
    int fd = open(path, O_RDONLY);
    if (-1 == fd) {
        printf("Error opening %s\n", path);
        return errno;
    } else {
        printEveryOther(fd);
        // rewind(fd);
        // nextLine(fd);
        // printEveryOther(fd);
        if (-1 == close(fd)) {
            printf("Error closing %s\n", path);
            return errno;
        } else {
            return 0;
        }
    }
}

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        printUsage();
        return EINVAL;
    } else {
        return oddsThenEvens(argv[1]);
    }
}
