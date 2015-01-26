CC=gcc
CFLAGS=-g -Wall -Werror
ALL=Odds_then-evens

.PHONY: all clean

all: $(ALL)

Odds_then-evens: p1.c
	$(CC) $(CFLAGS) -o $@ $^

AlrmTest: p4.c
	$(CC) $(CFLAGS) -o $@ $^

clean: clean-targets clean-objs

clean-objs:
	rm -rf *.o

clean-targets:
	rm -rf $(ALL)
