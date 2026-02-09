#!/usr/bin/env bash

# Copyright (C) 2023-2026 the Eask authors.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

## Commentary:
#
# Copy global .eask over to ~/.eask/
#

echo "Copy test .eask"
cp -R ./test/fixtures/home/_eask/ ~/.eask
cp -R ./test/fixtures/home/Eask ~/Eask

echo "Copy test configuration"
cp -R ./test/fixtures/home/.emacs.d/ ~/.emacs.d
