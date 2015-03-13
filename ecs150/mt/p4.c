#include <stdio.h>

int main()
{
	alarm(2);
	printf("still going\n");
	while (1);
	printf("should this line be executed?\n");
}
