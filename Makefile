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

command-analyze:
	./test/commands/analyze/dsl/run.sh
	./test/commands/analyze/metadata/run.sh
	./test/commands/analyze/error/run.sh

command-bump:
	./test/commands/bump/run.sh

command-compile:
	./test/commands/compile/run.sh

command-concat:
	./test/commands/concat/run.sh

command-docker:
	./test/commands/docker/run.sh

command-docs:
	./test/commands/docs/run.sh

command-eval:
	./test/commands/eval/run.sh

command-exec:
	./test/commands/exec/run.sh

command-emacs:
	./test/commands/emacs/run.sh

command-format:
	./test/commands/format/run.sh

command-info:
	./test/commands/info/run.sh

command-init:
	./test/commands/init/run.sh

command-install:
	./test/commands/install/run.sh

command-link:
	./test/commands/link/run.sh

command-lint:
	./test/commands/lint/run.sh

command-outdated-upgrade:
	./test/commands/outdated_upgrade/run.sh

command-recipe:
	./test/commands/recipe/run.sh

command-run:
	./test/commands/run/run.sh

command-search:
	./test/commands/search/run.sh

command-source:
	./test/commands/source/run.sh

test-ert:
	./test/commands/test/ert/run.sh

test-ert-runner:
	./test/commands/test/ert-runner/run.sh

test-buttercup:
	./test/commands/test/buttercup/run.sh

test-ecukes:
	./test/commands/test/ecukes/run.sh
