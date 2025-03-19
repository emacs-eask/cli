#!/usr/bin/env bash

# Copyright (C) 2025 the Eask authors.

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
# Test command/source errors
#

set -e

source ./test/fixtures/home/scripts/testing.sh

cd $(dirname "$0")

echo "Testing source command... errors"
should_error eask source
should_error eask source add
should_error eask source add foo # name not found, show help

should_error eask source delete
should_error eask source delete foo # name not found, show help
