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

test-exit-status:
	./test/exit-status/clean/run.sh
	./test/exit-status/compile/run.sh
	./test/exit-status/create/run.sh
	./test/exit-status/eval/run.sh
	./test/exit-status/format/run.sh
	./test/exit-status/generate/run.sh
	./test/exit-status/install/run.sh
	./test/exit-status/install-deps/run.sh
	./test/exit-status/link/run.sh
	./test/exit-status/lint/run.sh
	./test/exit-status/load/run.sh
	./test/exit-status/package/run.sh
	./test/exit-status/refresh/run.sh
	./test/exit-status/reinstall/run.sh
	./test/exit-status/run/run.sh
	./test/exit-status/test/run.sh
	./test/exit-status/uninstall/run.sh
