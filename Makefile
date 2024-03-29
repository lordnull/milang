PROJECT = milang
PROJECT_DESCRIPTION = I'm crazy so I'm writting a language
PROJECT_VERSION = 0.1.0

ERLANG_OTP = OTP-25.0

DEPS = getopt

SHELL_OPTS = +pc unicode

system_headers = Concurrency.milang-header Core.milang-header System.Print.milang-header

system_headers/%.milang-header : priv/system_headers/%.milang-header
	cp priv/system_headers/* system_headers/

include erlang.mk

ESCRIPT_NAME = milangc
ESCRIPT_FILE = milangc

escript-zip:: system_headers/*.milang-header archive_support/milang_curry.beam archive_support/milang_bootstrap.erl
	$(verbose) $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) system_headers/* archive_support/*

priv/archive_support/milang_curry.beam : ebin/milang_curry.beam
	cp $< $@

archive_support/milang_bootstrap.erl : priv/archive_support/milang_bootstrap.erl
	cp $< $@

archive_support/milang_curry.beam : priv/archive_support/milang_curry.beam
	cp $< $@
