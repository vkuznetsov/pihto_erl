REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run: compile
	@( erl -pa $PWD/ebin deps/*/ebin -eval "application:ensure_all_started(webserver), sync:go()." )

.PHONY: all deps compile clean run
