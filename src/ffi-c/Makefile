.PHONY: clean

bin/mandelpng : src/*
	gprbuild

clean :
	gprclean
	rm -f *.png
	find . -name "*~" -delete
