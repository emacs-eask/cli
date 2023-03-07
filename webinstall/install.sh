#!/bin/sh

# Copyright 2023 the Eask authors.

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
# TODO(everyone): Keep this script simple and easily auditable.
#

set -e

if ! command -v unzip >/dev/null; then
  echo "Error: unzip is required to install Eask." 1>&2
  exit 1
fi

if [ "$OS" = "Windows_NT" ]; then
  target="win-x64"
else
  case $(uname -sm) in
    "Darwin x86_64") target="macos-x64" ;;
    "Darwin arm64") target="macos-arm64" ;;
    "Linux aarch64")
      echo "Error: Official Deno builds for Linux aarch64 are not available. (see: https://github.com/denoland/deno/issues/1846 )" 1>&2
      exit 1
      ;;
    *) target="linux-x64" ;;
  esac
fi

eask_uri="https://github.com/emacs-eask/binaries/raw/master/${target}.zip"

eask_bin_dir=~/.local/bin
zip=$eask_bin_dir/eask.zip

mkdir -p $eask_bin_dir

curl -fsSL $eask_uri -o $zip

unzip -d "$eask_bin_dir" -o "$zip"

rm $zip

echo "✓ Eask is installed in %EAKS_BIN_DIR%."
echo
echo "Don't forget to add ${eask_bin_dir} to PATH environment variable:"
echo
echo "    export PATH=\"${eask_bin_dir}:\$PATH\""

