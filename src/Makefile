# this Makefile is used only for the cleanup

.PHONY: clean

SOURCES = $(wildcard *.adb)
SOURCES += $(wildcard **/*.adb)
TRASH = $(wildcard **/*.o)
TRASH += $(wildcard **/*.ali)
TRASH += $(wildcard **/*~)

EXE = $(patsubst %.adb,%,$(SOURCES))


clean :
	rm -f *~ *.ali *.o $(TRASH)
	rm -f $(EXE)

