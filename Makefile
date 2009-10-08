PARSER = deps/erlydtl/src/erlydtl/erlydtl_parser.erl

ERL = $(notdir $(wildcard deps/mochiweb/src/*.erl) \
               $(wildcard deps/erlydtl/src/erlydtl/*.erl) \
               $(wildcard src/*.erl) \
               $(wildcard src/readers/*.erl) \
               $(wildcard src/behaviours/*.erl) \
               $(wildcard src/resolvers/*/*.erl) \
               $(PARSER))
BEAM = $(ERL:%.erl=ebin/%.beam)
APP = ebin/playdar.app ebin/mochiweb.app ebin/erlydtl.app

vpath %.erl src
vpath %.erl src/readers
vpath %.erl src/behaviours
vpath %.erl src/resolvers/fake
vpath %.erl src/resolvers/lan
vpath %.erl src/resolvers/library
vpath %.erl src/resolvers/script
vpath %.erl deps/erlydtl/src/erlydtl
vpath %.erl deps/mochiweb/src

ebin/%.beam: %.erl | ebin
	erlc -o ebin -pa ebin +debug_info -W -I include $<

.PHONY: all clean

all: $(BEAM) ebin/erlydtl_parser.beam $(APP)

$(PARSER): $(dir $(PARSER))/erlydtl_parser.yrl | ebin
	erlc -o $(dir $(PARSER)) $<

ebin/playdar.app: src/playdar.app | ebin
	cp $< $@
ebin/mochiweb.app: deps/mochiweb/src/mochiweb.app | ebin
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
