CC=gcc
CFLAGS=-g -Wall -Werror
ALL=odds-then-evens

.PHONY: all clean

all: $(ALL)

odds-then-evens: p1.c
	$(CC) $(CFLAGS) -o $@ $^

clean: clean-targets clean-objs

clean-objs:
	rm -rf *.o

clean-targets:
	rm -rf $(ALL)
