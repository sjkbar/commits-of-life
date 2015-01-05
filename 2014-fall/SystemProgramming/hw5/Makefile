CC=gcc -m32
CFLAGS=-O2 -Wall
LIBS=-lpthread

FILES = Makefile test.c csapp.h csapp.c

PROGS = test

all: $(PROGS)

test: test.c csapp.o
	$(CC) $(CFLAGS) $(LIBS) -o client test.c csapp.o

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

clean:
	rm -f *.o *~ *.exe *.tar
	rm -f $(PROGS)
