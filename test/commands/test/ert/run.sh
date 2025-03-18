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
# Test command `ert`
#

set -e

source ./test/fixtures/home/scripts/testing.sh

echo "Test command 'ert'..."
cd $(dirname "$0")

eask test ert ./test/*.el

# regression
echo "Test ert: nil message"
eask test ert ./test-nil-message/*.el

echo "Testing ert command... no files"
should_error eask test ert
