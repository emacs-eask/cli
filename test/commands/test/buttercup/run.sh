#!/usr/bin/env bash

# Copyright (C) 2022-2024 the Eask authors.

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
# Test command `buttercup`
#

set -e

echo "Test command 'buttercup'..."
cd $(dirname "$0")

eask install-deps --dev
if eask test buttercup; then
    # this runs all tests, so should error
    echo "expected error"
    exit 1
fi

# buttercup takes directories as arguments
eask test buttercup ./test-ok
if eask test buttercup ./test-ok ./test-fail; then
    echo "expected error"
    exit 1
fi

# buttercup does not take options
eask test buttercup --no-color ./test-ok

# Because load-path is manually set, cannot refer to parent directories.
# Note this does work if you do ../buttercup/test-ok/, but not for any other directory.
if eask test buttercup ../ert/ 2>&1 | grep 'No suites defined'; then
    echo "expected error"
    exit 1
fi
