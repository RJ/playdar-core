PARSER = playdar/deps/erlydtl/src/erlydtl/erlydtl_parser.erl

ERL = $(notdir $(wildcard mochiweb-svn/src/*.erl) \
               $(wildcard playdar/deps/erlydtl/src/erlydtl/*.erl) \
               $(wildcard playdar/src/*.erl) \
               $(wildcard playdar/src/readers/*.erl) \
               $(wildcard playdar/src/behaviours/*.erl) \
               $(wildcard playdar/src/resolvers/*/*.erl) \
               $(PARSER))
BEAM = $(ERL:%.erl=ebin/%.beam)
APP = ebin/playdar.app ebin/mochiweb.app ebin/erlydtl.app

vpath %.erl playdar/src
vpath %.erl playdar/src/readers
vpath %.erl playdar/src/behaviours
vpath %.erl playdar/src/resolvers/fake
vpath %.erl playdar/src/resolvers/lan
vpath %.erl playdar/src/resolvers/library
vpath %.erl playdar/src/resolvers/script
vpath %.erl playdar/deps/erlydtl/src/erlydtl
vpath %.erl mochiweb-svn/src

ebin/%.beam: %.erl | ebin
	erlc -o ebin -pa ebin +debug_info -W -I playdar/include $<

.PHONY: all clean

all: $(BEAM) ebin/erlydtl_parser.beam $(APP)

$(PARSER): $(dir $(PARSER))/erlydtl_parser.yrl | ebin
	erlc -o $(@D) $<

ebin/playdar.app: playdar/src/playdar.app | ebin
	cp $< $@
ebin/mochiweb.app: mochiweb-svn/src/mochiweb.app | ebin
	cp $< $@
ebin/erlydtl.app: $(dir $(PARSER))/erlydtl.app | ebin
	cp $< $@

ebin:
	mkdir ebin

clean:
	rm -f $(BEAM)
	rm -f $(APP)
	test -d ebin && rmdir ebin
	rm -f $(PARSER)
