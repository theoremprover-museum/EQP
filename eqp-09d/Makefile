#
#  UNIX makefile for building eqp
#
#  DFLAGS controls conditional compilation:
#    -DTP_RUSAGE 

DFLAGS = -DTP_RUSAGE

#C compiler
CC = gcc

# CFLAGS = -g $(DFLAGS)
CFLAGS = -O $(DFLAGS)
# CFLAGS = -pg -O $(DFLAGS)

FILES =   clocks.c avail.c term.c misc.c symbols.o io.c options.c unify.c ac.c dioph.c btu.c btm.c demod.c discrim.c fpa.c list.c clause.c paramod.c eqp.c lrpo.c pindex.c interp.c

OBJECTS = clocks.o avail.o term.o misc.o io.o symbols.o options.o unify.o ac.o dioph.o btu.o btm.o demod.o discrim.o fpa.o list.o clause.o paramod.o eqp.o lrpo.o pindex.o interp.o

ttest: ttest.o avail.o stats.o options.o symbols.o misc.o clocks.o term.o list.o io.o unify.o discrim.o fpa.o clause.o lrpo.o btm.o dioph.o ac.o btu.o demod.o paramod.o pindex.o interp.o eqp.o
	$(CC) $(CFLAGS) -o ttest ttest.o avail.o stats.o options.o symbols.o misc.o clocks.o term.o list.o io.o unify.o discrim.o fpa.o clause.o lrpo.o btm.o dioph.o ac.o btu.o demod.o paramod.o pindex.o interp.o eqp.o

eqp: main.o avail.o stats.o options.o symbols.o misc.o clocks.o term.o list.o io.o unify.o discrim.o fpa.o clause.o lrpo.o btm.o dioph.o ac.o btu.o demod.o paramod.o pindex.o interp.o eqp.o
	$(CC) $(CFLAGS) -o eqp09d main.o avail.o stats.o options.o symbols.o misc.o clocks.o term.o list.o io.o unify.o discrim.o fpa.o clause.o lrpo.o btm.o dioph.o ac.o btu.o demod.o paramod.o pindex.o interp.o eqp.o

objects: $(OBJECTS)
	echo "ok, objects made."

clean:
	/bin/rm *.o

main.o $(OBJECTS): Header.h
main.o $(OBJECTS): Clocks.h
main.o $(OBJECTS): Term.h
main.o $(OBJECTS): List.h
main.o $(OBJECTS): Symbols.h
main.o $(OBJECTS): Unify.h
main.o $(OBJECTS): Discrim.h
main.o $(OBJECTS): Fpa.h
main.o $(OBJECTS): Clause.h

main.o $(OBJECTS): Ac.h
main.o $(OBJECTS): Demod.h
main.o $(OBJECTS): Order.h
main.o $(OBJECTS): Paramod.h

main.o eqp.o: Eqp.h
