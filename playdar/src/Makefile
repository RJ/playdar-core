include ../support/include.mk

DIRS    = readers resolvers behaviours

all: $(EBIN_FILES_NO_DOCS)
	cd behaviours; $(MAKE)
	cd readers; $(MAKE)
	cd resolvers; $(MAKE)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
