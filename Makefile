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
