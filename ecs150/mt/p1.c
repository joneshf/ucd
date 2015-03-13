#include <stdio.h>
#include <unistd.h>

int main()
{
	int id;
	printf("here comes the date.\n");
	if (id = fork() == 0) {
		printf ("PID is %d and ID  is %d\n", getpid (), id);
		execl ("/bin/date", "date", 0);
	}
	printf ("that was the date.\n");
}
