SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: test-global test-local test-redefine

#
## Commands
test-commands: test-global test-local
test-commands-dos: test-global-dos test-local-dos

test-global:
	@echo "Test global commands..."
	$(SHELL) ./test/commands/test_global.sh

test-global-dos:
	@echo "Test global commands..."
	./test/commands/test_global.bat

test-local:
	@echo "Test local commands..."
	$(SHELL) ./test/commands/test_local.sh

test-local-dos:
	@echo "Test local commands..."
	$(SHELL) ./test/commands/test_local.bat

test-exec:
	@echo "Test command exec..."
	$(SHELL) ./test/commands/exec/make.sh

test-exec-dos:
	@echo "Test command exec..."
	./test/commands/exec/make.bat

#
## Development
test-redefine:
	@echo "Test redefine..."
	$(EASK) concat
	$(EASK) load ./test/development/compile/test-redefine.el
