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
# Test eask init errors
#

set -e

source ./test/fixtures/home/scripts/testing.sh

cd $(dirname "$0")

echo "Testing init --from cask command... no files"
should_error eask init --from cask

echo "Testing init --from keg command... no files"
should_error eask init --from keg

echo "Testing init --from eldev command... no files"
should_error eask init --from eldev

echo "Testing init --from source command... no files"
should_error eask init --from source
