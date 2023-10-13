# Copyright (C) 2023 Jen-Chieh Shen

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
# Copy configuration over to ~/.emacs.d/
#

echo "Copy test configuration"
mkdir "$env:USERPROFILE/AppData/Roaming/.eask"
robocopy /e "./test/fixtures/home/_eask/" "$env:USERPROFILE/AppData/Roaming/.eask"
robocopy /e "./test/fixtures/home/Eask" "$env:USERPROFILE/AppData/Roaming/Eask"

echo "Copy test configuration"
mkdir "$env:USERPROFILE/AppData/Roaming/.emacs.d"
robocopy /e "./test/fixtures/home/.emacs.d/" "$env:USERPROFILE/AppData/Roaming/.emacs.d"
