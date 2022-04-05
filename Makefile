SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: test-redefine

#
## Development
test-redefine:
	@echo "Test redefine..."
	$(EASK) clean-all
	$(EASK) concat
	$(EASK) load ./test/development/compile/test-redefine.el
