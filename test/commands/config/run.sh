#!/usr/bin/env bash

# Copyright (C) 2022-2025 the Eask authors.

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
# Here we test all config (~/.emacs.d/) that the Emacser can be use daily!
#
# Notice, to make config commands work; we need a minimum configuration
# (home/.emacs.d), and place it under to the default Emacs directory!
#

set -e

echo "Copy test configuration"
./test/fixtures/home/scripts/setup.sh

echo "Testing config (~/.emacs.d/) commands..."
eask archives  -c

eask install   -c spinner ivy beacon company fuzzy
eask uninstall -c ivy fuzzy

eask list      -c --depth=0
eask outdated  -c
