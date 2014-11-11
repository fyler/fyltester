ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
APPNAME = fyltester
ERL_LIBS:=apps:deps


ERL=erl +A 4 +K true
ifeq (,$(wildcard ./rebar))
	REBAR := $(shell which rebar)
else
	REBAR := ./rebar
endif


all: deps update_deps compile

update:
	git pull

deps:
	@$(REBAR) get-deps

update_deps:
	@$(REBAR) update-deps

compile:
	@$(REBAR) compile

release: clean compile
	@$(REBAR) generate force=1

soft-release:
	@$(REBAR) generate force=1

clean:
	@$(REBAR) clean

run:
	ERL_LIBS=apps:deps erl -args_file files/vm.args -sasl errlog_type error -boot start_sasl -s $(APPNAME) -embedded -config files/app.config