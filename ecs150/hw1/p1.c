#include <errno.h>
#include <stdio.h>

void printUsage()
{
    printf("Usage: odds-then-evens <file>\n\n");
    printf("Prints the odd lines in a file followed by the even lines.\n");
}

void printLine(FILE * fp)
{
    char c = fgetc(fp);
    while ('\n' != c) {
        printf("%c", c);
        c = fgetc(fp);
    }
    printf("\n");
}

void nextLine(FILE * fp)
{
    char c = fgetc(fp);
    while ('\n' != c && EOF != c) {
        c = fgetc(fp);
    }
}

void printEveryOther(FILE * fp)
{
    char c = fgetc(fp);
    fseek(fp, -1, SEEK_CUR);
    while (EOF != c) {
        printLine(fp);
        nextLine(fp);
        c = fgetc(fp);
        fseek(fp, -1, SEEK_CUR);
    }
}

int oddsThenEvens(char const * path)
{
    FILE * fp = fopen(path, "r");
    if (fp == NULL) {
        printf("Error opening %s\n", path);
        return errno;
    } else {
        printEveryOther(fp);
        rewind(fp);
        nextLine(fp);
        printEveryOther(fp);
        fclose(fp);
    }
    return 0;
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
