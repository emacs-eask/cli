#!/usr/bin/env bash

# Copyright (C) 2022-2023 Jen-Chieh Shen

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Commentary:
#
# Here we test all local (workspace) commands by simulating a Emacs
# pacakge development environment!
#
# Notice, below we clone a random packae (repo) that uses Eask as the
# dependencies management tool.
#

set -e

# Naviate to the test package
cd "./test/fixtures/mini.emacs.pkg.1/"

echo "Testing local commands..."
eask info
eask status
eask archives
eask archives --all
eask list --depth=0
eask cat package.json --insecure
eask cat package.json --number --insecure
eask concat
eask bump major minor patch

# PATH environment
eask path
eask path      bin
eask load-path
eask load-path bin

# Preparation
eask prepare --dev
eask package

# Development
eask compile
eask compile --clean
eask recipe
eask keywords
eask run script
eask run script test
eask run script -all
eask run command
eask run command test
eask run command -all

# Exection
eask eval "(progn (require 'mini.emacs.pkg.1))"

# Generation
eask generate autoloads
eask generate pkg-file
eask generate recipe -y
#eask generate license gpl-3.0  # XXX: Avoid API rate limit exceeded error
eask generate ignore elisp

# Linting
eask lint checkdoc
eask lint declare
eask lint elint
eask lint elisp-lint
#eask lint elsa                 # XXX: Elsa is not stable, ignore it for now
eask lint indent
eask lint keywords
eask lint license
eask lint package
eask lint regexps

# Testing
eask test activate

# Cleaning
eask clean .eask
eask clean elc
eask clean dist
eask clean autoloads
eask clean pkg-file
eask clean log-file
eask clean all

# Control DSL
eask source add test "https://test.elpa.com"
eask source delete test
eask source list

# Util
eask locate
eask upgrade-eask
eask refresh
