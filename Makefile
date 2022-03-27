SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: install test-global test-local

test: install test-global test-local

install:
	@echo "Preparing Eask..."
	npm install

test-global:
	@echo "Test global commands..."
	sh ./test/test_global.sh

test-local:
	@echo "Test local commands..."
	sh ./test/test_local.sh
