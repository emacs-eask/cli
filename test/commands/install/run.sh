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
# Test commands related to install, and uninstall
#

echo "Test commands related to install, and uninstall"

# Naviate to the test package
cd "./test/mini.emacs.pkg/"

echo "Install dependencies"
eask install-deps --verbose 4

# echo "Install project package"
# eask package
# eask install

# echo "Install by sepcifying packages"
# eask install beacon company-fuzzy lsp-ltex

# echo "Uninstall by sepcifying packages"
# eask uninstall beacon lsp-ltex

# echo "Uninstall project package"
# eask uninstall
