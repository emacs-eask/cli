#!/usr/bin/env bash

# Copyright (C) 2023-2025 the Eask authors.

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

eask link add "mini.pkg.1" "./test/fixtures/mini.pkg.1/"
eask link list
eask link delete mini.pkg.1-0.0.1
