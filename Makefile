ifeq ($(OS),Windows_NT)
SHELL :=
else
SHELL := /usr/bin/env bash
endif

EMACS ?= emacs
EASK ?= eask

.PHONY: test-global test-local test-redefine

#
## Commands
test-commands: test-global test-local

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
	$(EASK) load ./test/development/compile/test-redefine.el
