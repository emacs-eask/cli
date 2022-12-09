#!/usr/bin/env bash

# Copyright (C) 2022 Jen-Chieh Shen

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

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
cd "./test/mini.emacs.pkg/"

echo "Testing local commands..."
eask info
eask archives
eask list --depth=0
eask concat

# Preparation
eask prepare --dev
eask package

# Development
eask compile
eask activate
eask recipe
eask keywords
eask run
eask run test
eask run -all

# Linter
eask lint package
eask lint checkdoc
eask lint declare
eask lint elsa
eask lint elint
eask lint indent
eask lint keywords
eask lint relint

# Clean up
eask clean
eask clean-elc
eask clean-all

# Util
eask locate
eask upgrade-eask
eask refresh
