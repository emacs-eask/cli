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
	$(EASK) load ./test/compat.el
