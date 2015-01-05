CC=gcc
CFLAGS= -Wall

FILES = Makefile c1.c. c2.c global.h

PROGS = test_0

all: $(PROGS)

test_0: c2.c c1.o_0
	$(CC) $(CFLAGS) c2.c c1.o global.h

c1.o_0: c1.c global.h
	$(CC) $(CFLAGS) -c c1.c global.h

test_c1: c2.c c1.o_c1
	$(CC) $(CFLAGS) c2.c c1.o global.h

c1.o_c1: c1.c global.h
	$(CC) $(CFLAGS) -c c1.c global.h -DINITIALIZE

test_c2: c2.c c2.o_c2
	$(CC) $(CFLAGS) c2.c c1.o global.h -DINITIALIZE

c2.o_c2: c1.c global.h
	$(CC) $(CFLAGS) -c c1.c global.h

test_both: c2.c c2.o_both
	$(CC) $(CFLAGS) c2.c c1.o global.h -DINITIALIZE

c2.o_both: c1.c global.h
	$(CC) $(CFLAGS) -c c1.c global.h -DINITIALIZE

clean:
	rm -f *.o *~ *.exe *.tar *.gch
	rm -f $(PROGS)
	rm -f *.out
