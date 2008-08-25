YAMD_VSN=1.0 beta 3

EMULATOR=beam
EBIN=$(ROOT)/ebin
INCLUDE=$(ROOT)/include
SCRIPT=$(ROOT)/scripts

APP_TARGET=$(EBIN)/yamd.app
ELOG_TARGET=$(EBIN)/elog.config
SH_TARGET=$(SCRIPT)/yamd

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	yamd_app.erl \
	yamd_sup.erl \
	yamd_tcp_acceptor.erl \
	yamd_tcp_client_sup.erl \
	yamd_tcp_interface.erl \
	yamd_cleaner.erl \
	yamd_store_manager.erl \
	yamd_store.erl \
	yamd_argument.erl \
	yamd_ctl.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(ELOG_TARGET) $(SH_TARGET)

TEST_SOURCES=yamd_test.erl
TEST_OBJECTS=$(TEST_SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/yamd.hrl

clean:
	rm -f $(OBJECTS) $(TEST_OBJECTS) $(EBIN)/Mnesia.* $(EBIN)/erl_crash.dump

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

$(APP_TARGET): yamd.app.src Makefile
	sed -e 's;%VSN%;$(YAMD_VSN);' $< > $@

$(ELOG_TARGET): elog.config.src Makefile
	sed -e 's;%LOGS_DIR%;$(ROOT)/logs;' $< > $@

$(SH_TARGET): yamd.src
	sed -e 's;%ROOT_DIR%;$(ROOT);' $< > $@
	chmod 755 $@

$(TEST_OBJECTS): $(TEST_SOURCES)
	erlc -pa $(EBIN) -pa $(EUNIT)/ebin -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -I$(EUNIT)/include -o$(EBIN) $<

test: $(TEST_OBJECTS)

