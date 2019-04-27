# this Makefile is used only for the cleanup

GPRS = $(wildcard **/*.gpr)

.PHONY: clean gprclean all $(GPRS)

SOURCES = $(wildcard *.adb)
SOURCES += $(wildcard **/*.adb)
TRASH = $(wildcard **/*.o)
TRASH += $(wildcard **/*.ali)
TRASH += $(wildcard **/*~)

EXE = $(patsubst %.adb,%,$(SOURCES))

all : clean gprclean

gprclean : $(GPRS)

$(GPRS) :
	gprclean -P $@


clean :
	rm -f *~ *.ali *.o $(TRASH)
	rm -f $(EXE)

