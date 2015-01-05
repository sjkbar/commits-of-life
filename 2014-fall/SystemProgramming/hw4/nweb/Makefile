CC=gcc -m32
CFLAGS=-O2 -Wall
LIBS=-lpthread

FILES = Makefile client.c nweb23.c csapp.h csapp.c

PROGS = client nweb23

all: $(PROGS)

client: client.c csapp.o csapp.h
	$(CC) $(CFLAGS) $(LIBS) -o client client.c csapp.o

nweb23: nweb23.c csapp.o csapp.h client
	$(CC) $(CFLAGS) $(LIBS) -o nweb23 nweb23.c csapp.o

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

clean:
	rm -f *.o *~ *.exe *.tar
	rm -f $(PROGS)
