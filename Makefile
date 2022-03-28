SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: install test-global test-local test-redefine

test-commands: install test-global test-local

install:
	@echo "Preparing Eask..."
	npm install

test-global:
	@echo "[Commands] Test global commands..."
	sh ./test/commands/test_global.sh

test-local:
	@echo "[Commands] Test local commands..."
	sh ./test/commands/test_local.sh

test-redefine:
	@echo "[Development] Test redefine..."
	$(EASK) concat
	echo ./test/development/test-redefine.el
