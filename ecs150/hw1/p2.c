#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

int usage()
{
    printf("Usage: connect\n\n");

    return EINVAL;
}

int pipeCreateError()
{
    printf("Error creating pipe.\n");

    return errno;
}

int printChildError()
{
    printf("Error forking child\n");
    return errno;
}

const char * strrev(char * str)
{
    size_t len = strlen(str);
    char rev[len];
    int i = 0, j = len - 1;

    while (i < len) {
        rev[i++] = str[j--];
    }

    const char * revPtr = rev;

    return revPtr;
}

const char * upper(char * str)
{
    size_t len = strlen(str);
    char upp[len];
    int i = 0;

    while (i < len) {
        upp[i] = toupper(str[i]);
        ++i;
    }

    const char * uppPtr = upp;

    return uppPtr;
}

const char * sanitize(char * str)
{
    size_t len = strlen(str);
    char san[len - 1];
    int i = 0;

    while ('\n' != str[i] && i + 1 < len) {
        san[i] = str[i];
        ++i;
    }

    san[i] = '\0';

    const char * sanPtr = san;

    return sanPtr;
}

int main(int argc, char const *argv[])
{
    if (argc != 1) {
        return usage();
    }

    int oneTwo[2], twoThree[2], twoOne[2], threeTwo[2];
    char buf12[4096], buf23[4096], buf13[8192];
    char * c12 = NULL;
    size_t len = 0;
    ssize_t lineLen;

    int p12 = pipe(oneTwo);
    if (-1 == p12) {
        return pipeCreateError();
    }
    int p23 = pipe(twoThree);
    if (-1 == p23) {
        return pipeCreateError();
    }
    int p21 = pipe(twoOne);
    if (-1 == p21) {
        return pipeCreateError();
    }
    int p32 = pipe(threeTwo);
    if (-1 == p32) {
        return pipeCreateError();
    }

    pid_t parent, child1, child2;
    parent = getpid();
    if (getpid() == parent) {
        child1 = fork();

        if (-1 == child1) {
            return printChildError();
        }
    }

    if (getpid() == parent) {
        child2 = fork();

        if (-1 == child2) {
            return printChildError();
        }
    }

    if (getpid() == parent) {
        // The parent only gets to write to `oneTwo`
        // and read from `twoOne`.
        close(oneTwo[0]);
        close(twoOne[1]);
        close(twoThree[0]);
        close(twoThree[1]);
        close(threeTwo[0]);
        close(threeTwo[1]);
    } else if (0 == child1) {
        // The first child reads from `oneTwo`,
        // writes to `twoThree`,
        // reads from `threeTwo`,
        // and writes to `twoOne`.
        close(oneTwo[1]);
        close(twoThree[0]);
        close(threeTwo[1]);
        close(twoOne[0]);
    } else if (0 == child2) {
        // The second child reads from `twoThree`,
        // and writes to `threeTwo`.
        close(oneTwo[0]);
        close(oneTwo[1]);
        close(twoOne[0]);
        close(twoOne[1]);
        close(twoThree[1]);
        close(threeTwo[0]);
    }

    if (getpid() == parent) {
        // Prompt for string
        printf("Please enter a string: ");
        lineLen = getline(&c12, &len, stdin);
        if (lineLen > 0) {
            const char * sanitized12 = sanitize(c12);
            // Print the string.
            printf("read this: %s\n", sanitized12);

            // Send it to the first child.
            write(oneTwo[1], sanitized12, lineLen);
            close(oneTwo[1]);
            // Wait for the first child's response.
            ssize_t readLen = read(twoOne[0], buf13, 8192);
            if (readLen > 0) {
                // Compare strings.
                printf("Comparing \"%s\" with \"%s\": %d\n",
                    sanitized12, buf13, strcmp(sanitized12, buf13)
                );
            }
            // Say good night.
            free(c12);
            printf("First process saying good bye\n");
        }
    } else if (0 == child1) {
        // Wait for the string from the parent.
        ssize_t readLen = read(oneTwo[0], buf12, 4096);
        if (readLen > 0) {
            // Reverse the string.
            const char * reversed = strrev(buf12);
            // Print the reversed string.
            printf("Reversed string: %s\n", reversed);
            // Send it to child2.
            write(twoThree[1], reversed, readLen);
            // Wait for child2's response.
            readLen = read(threeTwo[0], buf23, 4096);
            if (readLen > 0) {
                // Concat strings together.
                strcat(buf13, buf12);
                strcat(buf13, buf23);
                // Print concatted strings.
                printf("Concatenated strings: %s\n", buf13);
                // Send it to the parent.
                write(twoOne[1], buf13, 8192);

                // Bid adieu.
                printf("Second process saying good bye\n");
            }
        }
    } else if (0 == child2) {
        // Wait for the string from the child1.
        ssize_t readLen = read(twoThree[0], buf12, 4096);
        if (readLen > 0) {
            // Uppercase the string.
            const char * upped = upper(buf12);
            // Print the new string.
            printf("Uppercased string: %s\n", upped);
            // Send it to child1.
            write(threeTwo[1], upped, readLen);
            // Au revoir.
            printf("Third process saying good bye\n");

        }
    }

    return 0;
}
