all:
	cd mochiweb-svn; $(MAKE)
	cd playdar; $(MAKE)

debug:

clean:
	cd mochiweb-svn; $(MAKE) clean
	cd playdar; $(MAKE) clean
