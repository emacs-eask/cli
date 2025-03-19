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
# Test commands related to install, and uninstall
#

set -e

source ./test/fixtures/home/scripts/testing.sh

echo "Test commands related to install, and uninstall"

# Naviate to the test package
cd "./test/fixtures/mini.pkg.1/"

echo "Install dependencies"
eask install-deps

echo "Install project package"
eask package
eask install

echo "Install by sepcifying packages"
eask install beacon company-fuzzy transwin

echo "Uninstall by sepcifying packages"
eask uninstall beacon transwin

echo "Uninstall project package"
eask uninstall

echo "Test eask install ... no files"
cd -
cd $(dirname "$0")
should_error eask install

echo "Test eask uninstall ... no files"
should_error eask uninstall

echo "Test eask reinstall ... no files"
should_error eask reinstall
