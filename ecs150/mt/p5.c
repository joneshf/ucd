#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
char phrase[] = "stuff that in your pipe";

int main()
{
	int fd [2], bytesread;
	char message [100];
	pipe (fd);


	if (fork() == 0)
	{
		close(fd[0]);
		write(fd[1],phrase, strlen(phrase)+1);
		while (1);

	}
	else
	{
		close (fd[1]);
		bytesread = read(fd[0], message, 100);
		printf("read %d bytes: %s\n", bytesread, message);
		bytesread = read(fd[0], message, 100);
		printf("read %d bytes: %s\n", bytesread, message);
		close (fd[0]);
	}
}
