
DIALYZER_OPTS = -Wrace_conditions

all: deps compile

deps:
	rebar get-deps

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
ERLANG_APPS=erts kernel stdlib crypto public_key mnesia ssl

# Be sure the compile first before running this or the deps
# directory won't be scanned by dialyzer.
$(DEPSOLVER_PLT):
		dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
					--apps $(ERLANG_APPS) -r deps

dialyzer: $(DEPSOLVER_PLT)
		dialyzer --plt $(DEPSOLVER_PLT) $(DIALYZER_OPTS) --src src

typer: $(DEPSOLVER_PLT)
		typer --plt $(DEPSOLVER_PLT) -r ./src

compile:
	rebar compile escriptize

test:
	tests/test.sh

clean:
	rebar clean

destclean: clean
	-rm -fr deps ebin $(DEPSOLVER_PLT)

.PHONY: dialyzer typer clean distclean deps compile
