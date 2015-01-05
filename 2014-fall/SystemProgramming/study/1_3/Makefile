CC=gcc
CFLAGS= -Wall

FILES = Makefile main.c main.h

PROGS = main

all: $(PROGS)

main: main.c
	$(CC) $(CFLAGS) main.c

clean:
	rm -f *.o *~ *.exe *.tar *.gch
	rm -f $(PROGS)
	rm -f *.out
