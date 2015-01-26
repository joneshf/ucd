#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int usage()
{
    printf("Usage: File-descriptor-test <file>\n\n");

    return EINVAL;
}


int main(int argc, char const *argv[])
{
    if (argc != 2) {
        return usage();
    }

    int fd = open(argv[1], O_RDONLY);
    pid_t pid = fork();

    if (0 == pid) {
        printf("Closing the file descriptor from the child.\n");
        sleep(1);
        close(fd);
        printf("Closed file descriptor.\n");
    } else {
        printf("Waiting for child to die.\n");
        int status;
        int * statusPtr = &status;
        waitpid(pid, statusPtr, 0);
        printf("Child dead, reading file: %s\n", argv[1]);
        char buf[10];
        ssize_t bytes = read(fd, buf, 10);
        printf("Read %d bytes: %s\n", (char)bytes, buf);
    }

    return 0;
}
