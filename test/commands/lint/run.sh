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
# Test command/lint errors
#

set -e

source ./test/fixtures/home/scripts/testing.sh

# Naviate to the test package
cd $(dirname "$0")

echo "Testing lint checkdoc command... no files"
should_error eask lint checkdoc

echo "Testing lint declare command... no files"
should_error eask lint declare

echo "Testing lint elint command... no files"
should_error eask lint elint

echo "Testing lint elisp-lint command... no files"
should_error eask lint elisp-lint

echo "Testing lint elsa command... no files"
should_error eask lint elsa

echo "Testing lint indent command... no files"
should_error eask lint indent

echo "Testing lint keywords command... no files"
should_error eask lint keywords

echo "Testing lint regexps command... no files"
should_error eask lint regexps
