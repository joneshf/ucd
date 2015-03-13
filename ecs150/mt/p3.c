#include <stdio.h>
#include <fcntl.h>


int main()
{
	int fd1, fd2, fd3;
	fd1 = open ("junk.txt", O_RDWR | O_TRUNC);
	printf("fd1 = %d\n", fd1);
	/* other statements, perhaps many */

	write(fd1, "let's ", 5);
	fd2 = dup2 (fd1, 1);
	printf (" fd2 = %d\n", fd2);
	write(fd2, " get ", 4);

	fd3 = dup2 (fd1, 0);
	printf(" fd3 = %d\n", fd3);
	write(0," going\n", 6);
	dup2(3, 2);
	write(2, " !\n", 2);
}
