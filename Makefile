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

test-options:
	./test/options/run.sh

color:
	./test/color/run.sh

error:
	./test/error/run.sh

command-global:
	./test/commands/global/run.sh

command-config:
	./test/commands/config/run.sh

command-local:
	./test/commands/local/run.sh

command-check-eask:
	./test/checker/dsl/run.sh
	./test/checker/metadata/run.sh

command-docker:
	./test/commands/docker/run.sh

command-exec:
	./test/commands/exec/run.sh

command-emacs:
	./test/commands/emacs/run.sh

command-install:
	./test/commands/install/run.sh

command-outdated-upgrade:
	./test/commands/outdated_upgrade/run.sh

command-search:
	./test/commands/search/run.sh

command-link:
	./test/commands/link/run.sh

test-ert:
	./test/commands/test/ert/run.sh

test-ert-runner:
	./test/commands/test/ert-runner/run.sh

test-buttercup:
	./test/commands/test/buttercup/run.sh
