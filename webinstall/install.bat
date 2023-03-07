@echo off

:: Copyright 2023 the Eask authors.

:: This program is free software; you can redistribute it and/or modify
:: it under the terms of the GNU General Public License as published by
:: the Free Software Foundation; either version 3, or (at your option)
:: any later version.

:: This program is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.

:: You should have received a copy of the GNU General Public License
:: along with GNU Emacs; see the file COPYING.  If not, write to the
:: Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
:: Boston, MA 02110-1301, USA.

::: Commentary:
::
:: TODO(everyone): Keep this script simple and easily auditable.
::

set URL=https://github.com/emacs-eask/binaries/raw/master/win-x64.zip
set EAKS_BIN_DIR=%USERPROFILE%\.local\bin
set ZIP=%EASK_BIN_DIR%\eask.zip

mkdir %EAKS_BIN_DIR%

curl.exe -fsSL %URL% -o %ZIP%

tar.exe -xf %ZIP% -C %EAKS_BIN_DIR%

del %ZIP%

echo ✓ Eask is installed in %EAKS_BIN_DIR%.
echo.
echo Don't forget to add `%EASK_BIN_DIR% to PATH environment variable:
echo.
echo     set PATH=%ELDEV_BIN_DIR%;%%PATH%%
