# this Makefile is used only for the cleanup

.PHONY: clean

SOURCES = $(wildcard *.adb)
EXE = $(patsubst %.adb,%,$(SOURCES))

clean :
	rm -f *.o *.ali *~
	rm -f $(EXE)

