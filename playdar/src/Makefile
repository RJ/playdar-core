include ../support/include.mk

#DIRS    = readers

all: $(EBIN_FILES)
	cd readers; $(MAKE)
	cd resolvers; $(MAKE)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
