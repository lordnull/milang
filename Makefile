PROJECT = milang
PROJECT_DESCRIPTION = I'm crazy so I'm writting a language
PROJECT_VERSION = 0.1.0

DEPS = getopt

system_headers = Concurrency.milang-header Core.milang-header System.Print.milang-header

system_headers/%.milang-header : priv/%.milang-header
	cp priv/* system_headers/

$(PROJECT).d:: system_headers/*.milang-header

include erlang.mk

ESCRIPT_NAME = milangc
ESCRIPT_FILE = milangc

escript-zip::
	$(verbose) $(ESCRIPT_ZIP) $(ESCRIPT_ZIP_FILE) system_headers/*
