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

npm install

TEST_PROJECT="https://github.com/jcs-elpa/parse-it"  # project use to test
CMD="node eask"

echo "[INFO] 'eask' version: "
$CMD --version

git clone $TEST_PROJECT
cd 'parse-it'
$CMD info
$CMD install
$CMD compile
$CMD lint
$CMD list --depth=0

$CMD clean
$CMD clean-elc
$CMD clean-all
