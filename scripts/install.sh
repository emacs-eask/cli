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

# get the .bashrc path
BASHRC_FILE=$HOME/.bashrc

# setup the environment into .bashrc file.
echo "adding path to .bashrc file..."
cat <<EOF >> $BASHRC_FILE
# eask install package
EASK_BIN='$PWD/bin'
export EASK_BIN
export PATH=$PATH:$EASK_BIN
EOF

echo "EASK_BIN='$PWD/bin'"
echo "export EASK_BIN"
echo "export PATH=$PATH:$EASK_BIN"

echo "reload .bashrc file..."
source ~/.bashrc

# See if `eask` install successfully!
eask --version
