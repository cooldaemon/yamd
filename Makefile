APPNAME=yamd
SUB_DIRECTORIES=src

EUNIT=$(HOME)/cean/erlang/lib/eunit-2007.0617

ifndef ROOT
  ROOT=$(shell pwd)
endif

all: subdirs

subdirs:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) $(MAKE)); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) $(MAKE) clean); \
	done
	rm -rf Mnesia.* erl_crash.dump logs/*

test: subdirs
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; ROOT=$(ROOT) EUNIT=$(EUNIT) $(MAKE) test); \
	done
	@echo Testing...
	erl -noshell -name yamd -boot start_sasl -config $(ROOT)/ebin/elog -pa $(ROOT)/ebin -pa $(EUNIT)/ebin -s yamd_test test -s init stop

