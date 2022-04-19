SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: test-redefine

#
## Development
test-redefine:
	@echo "Test redefine..."
	$(EASK) concat
	$(EASK) load ./test/development/compile/test-redefine.el

test-compat:
	@echo "Test compatibility..."
	$(EASK) load ./test/development/compat.el --allow-error

color:
	./test/color/run.sh

error:
	./test/error/run.sh

command_global:
	./test/commands/global/run.sh

command_local:
	./test/commands/local/run.sh

command_check_eask:
	./test/checker/check-eask/run.sh

command_exec:
	./test/commands/exec/run.sh

command_install:
	./test/commands/install/run.sh

command_outdated_upgrade:
	./test/commands/outdated_upgrade/run.sh

command_search:
	./test/commands/search/run.sh
