#!/usr/bin/env bash

# Copyright (C) 2022 Jen-Chieh Shen

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

## Commentary:
#
# Test all options flag
#

set -e

echo "Test all options flag"
cd $(dirname "$0")

# Please have these flags in order, see `eask` file in the project root!
eask info -g
eask info --global
eask info -q
eask info --qucik
eask info -f
eask info --force
eask info --dev
eask info --development
eask info --debug
eask info --strict
eask info --allow-error
eask info --insecure
eask info --timestamps
eask info --log-level
eask info --elapsed-time
eask info --et
eask info --no-color
eask info --proxy localhost:8080
eask info --http-proxy localhost:8080
eask info --https-proxy localhost:8080
eask info --no-proxy localhost:8080
eask info -v 4
eask info --verbose 4
