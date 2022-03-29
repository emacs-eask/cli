SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: install test-global test-local test-redefine

install:
	@echo "Preparing Eask..."
	npm install
ifeq ($(OS),Windows_NT)
	cmd /C "./scripts/install.bat"
else
	$(SHELL) ./scripts/install.sh
endif

#
## Commands
test-commands: install test-global test-local

test-global:
	@echo "Test global commands..."
	$(SHELL) ./test/commands/test_global.sh

test-local:
	@echo "Test local commands..."
	$(SHELL) ./test/commands/test_local.sh

test-exec:
	@echo "Test command exec..."
	$(SHELL) ./test/commands/exec/make.sh

#
## Development
test-redefine:
	@echo "Test redefine..."
	$(EASK) concat
	$(EASK) load ./test/development/test-redefine.el
