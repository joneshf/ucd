#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

long count = 0;

void printInfo(int signum)
{
    printf("Count is %ld, my pid is %d\n", count, getpid());
    count = 0;
}

int printChildError()
{
    printf("Error forking child\n");
    return errno;
}

int main(int argc, char const *argv[])
{
    pid_t parent = getpid();
    pid_t child1, child2;

    child1 = fork();
    if (-1 == child1) {
        return printChildError();
    }
    if (getpid() == parent) {
        child2 = fork();
        if (-1 == child2) {
            return printChildError();
        }
    }

    if (0 == child1 || 0 == child2) {
        signal(SIGALRM, printInfo);
        while (1) {
            ++count;
        }
    } else {
        int i = 0;
        for (; i < 6; ++i) {
            sleep(1);
            kill(child1, SIGALRM);
            kill(child2, SIGALRM);
        }
    }
    if (getpid() == parent) {
        kill(child1, SIGKILL);
        kill(child2, SIGKILL);
    }
    return 0;
}
