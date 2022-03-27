SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: test-global test-local

test: test-global test-local

test-global:
	@echo "Test global commands..."
	sh ./test/test_global.sh

test-local:
	@echo "Test local commands..."
	sh ./test/test_local.sh
