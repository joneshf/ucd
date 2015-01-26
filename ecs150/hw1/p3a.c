#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char const *argv[])
{
    char * foo = "wat";
    pid_t pid = fork();

    if (0 == pid) {
        printf("From the child, foo: %s\n", foo);
        sleep(1);
        printf("Setting foo to \"yup\" in the child\n");
        foo = "yup";
        printf("From the child, foo: %s\n", foo);
    } else {
        int status;
        int * statusPtr = &status;
        printf("From the parent, foo: %s\n", foo);
        printf("Waiting for the child to die\n");
        waitpid(pid, statusPtr, 0);
        printf("Child dead, foo never changed in the parent: %s\n", foo);
    }

    return 0;
}
