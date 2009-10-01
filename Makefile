all: | playdar/ebin
	cd mochiweb-svn; $(MAKE)
	cd playdar; $(MAKE)

# note really this should be in every Makefile, but this will work usually
playdar/ebin:
	mkdir -p $@

debug:

clean:
	cd mochiweb-svn; $(MAKE) clean
	cd playdar; $(MAKE) clean
