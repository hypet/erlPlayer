REBAR_CONFIG:=$(PWD)/rebar.config
INCLUDE_DIR:=include
SRC_DIR:=src

compile: clean
	@./rebar compile

clean:
	@./rebar clean
	@find $(PWD)/. -name "erl_crash\.dump" | xargs rm -f
	
get-deps:
	@./rebar get-deps

run:
	@ERL_LIBS="."
	@erl -pa $(PWD)/ebin deps/*/ebin -I include -sname erlplayer -boot start_sasl -s erlplayer -sasl errlog_type error -config sys
	
all: clean get-deps compile run
