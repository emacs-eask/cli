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
# Test command `exec`
#

set -e

source ./test/fixtures/home/scripts/testing.sh

echo "Test command 'exec'..."
cd $(dirname "$0")

eask install-deps
eask exec ert-runner -h
eask exec github-elpa -h
eask exec echo hello world

eask exec buttercup -L .
eask exec buttercup -L . --pattern 'pattern 1'

echo "Testing exec command... no files"
should_error eask exec
