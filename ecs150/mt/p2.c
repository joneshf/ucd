#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>

int  main(void) {
	pid_t childpid;
	pid_t mypid;
	pid_t myparentspid;

	mypid = getpid();
	myparentspid = getppid();
	childpid = fork();

	if (childpid == 0)
		printf ("ID(myPID_1): %ld, ID(myPID_2): %ld and Parent PID: %ld\n", (long int) getpid(), (long int) getppid(), (long int) myparentspid);

	else
		printf ("ID(myPID_1): %ld, ID(myPID_2): %ld and Parent PID: %ld\n", (long int) getpid(), (long int) getppid(), (long int) myparentspid);

	return (0);
}

