ERLYDTL_D = deps/erlydtl/src/erlydtl
ERLYDTL_PARSER = $(ERLYDTL_D)/erlydtl_parser.erl

ERL = $(notdir $(wildcard deps/mochiweb/src/*.erl) \
               $(wildcard deps/erlydtl/src/erlydtl/*.erl) \
               $(wildcard src/*.erl) \
               $(wildcard src/behaviours/*.erl) \
               $(ERLYDTL_PARSER))
BEAM = $(ERL:%.erl=ebin/%.beam)

MODULES_ERL = $(wildcard playdar_modules/*/src/*.erl)
MODULES_BEAM_TMP = $(subst src,ebin,$(MODULES_ERL))
MODULES_BEAM = $(MODULES_BEAM_TMP:erl=beam)

APP = ebin/playdar.app ebin/mochiweb.app ebin/erlydtl.app
CFLAGS = -pa ebin +debug_info -W -I include

vpath %.erl src
vpath %.erl $(wildcard src/*)
vpath %.erl deps/mochiweb/src
vpath %.erl deps/erlydtl/src/erlydtl

ebin/%.beam: %.erl | ebin
	erlc $(CFLAGS) -o ebin $<

.PHONY: all clean

all: $(BEAM) $(MODULES_BEAM) ebin/erlydtl_parser.beam $(APP)

$(ERLYDTL_PARSER): $(ERLYDTL_D)/erlydtl_parser.yrl | ebin
	erlc -o $(ERLYDTL_D) $<

ebin/playdar.app: src/playdar.app | ebin
	cp $< $@
ebin/mochiweb.app: deps/mochiweb/src/mochiweb.app | ebin
	cp $< $@
ebin/erlydtl.app: $(ERLYDTL_D)/erlydtl.app | ebin
	cp $< $@

# for the moment we have to hardcode it all
playdar_modules/basic_readers/ebin:
	mkdir -p $@
playdar_modules/basic_readers/ebin/%.beam: playdar_modules/basic_readers/src/%.erl | playdar_modules/basic_readers/ebin
	erlc $(CFLAGS) -o $(@D) $<
playdar_modules/fake/ebin:
	mkdir -p $@
playdar_modules/fake/ebin/%.beam: playdar_modules/fake/src/%.erl | playdar_modules/fake/ebin
	erlc $(CFLAGS) -o $(@D) $<
playdar_modules/lan/ebin:
	mkdir -p $@
playdar_modules/lan/ebin/%.beam: playdar_modules/lan/src/%.erl | playdar_modules/lan/ebin
	erlc $(CFLAGS) -o $(@D) $<
playdar_modules/library/ebin:
	mkdir -p $@
playdar_modules/library/ebin/%.beam: playdar_modules/library/src/%.erl | playdar_modules/library/ebin
	erlc $(CFLAGS) -o $(@D) $<

ebin:
	mkdir ebin

clean:
	rm -rf ebin
	rm -rf playdar_modules/basic_readers/ebin
	rm -rf playdar_modules/fake/ebin
	rm -rf playdar_modules/library/ebin
	rm -rf playdar_modules/lan/ebin
	rm -f $(ERLYDTL_PARSER)
