CC=gcc
CFLAGS=-g -Wall -Werror
ALL=Odds_then-evens connect Fork-variable-test File-descriptor-test AlrmTest \
	directory_traverse_depth_first process_tree

.PHONY: all clean

all: $(ALL)

Odds_then-evens: p1.c
	$(CC) $(CFLAGS) -o $@ $^

connect: p2.c
	$(CC) $(CFLAGS) -o $@ $^

Fork-variable-test: p3a.c
	$(CC) $(CFLAGS) -o $@ $^

File-descriptor-test: p3b.c
	$(CC) $(CFLAGS) -o $@ $^

AlrmTest: p4.c
	$(CC) $(CFLAGS) -o $@ $^

directory_traverse_depth_first: p5.c
	$(CC) $(CFLAGS) -o $@ $^

process_tree: p6.c
	$(CC) $(CFLAGS) -o $@ $^

clean: clean-targets clean-objs

clean-objs:
	rm -rf *.o

clean-targets:
	rm -rf $(ALL)
