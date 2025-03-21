#!/usr/bin/env bash

# Copyright (C) 2025 the Eask authors.

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
# Test reporting functions
# This file will be sourced by other shell test scripts.
# Expects to run with set -e

# TODO: note that eask output is always to stderr

# Run command and exit with the same status
# All args interpreted as program to run
# e.g. should_run cmd arg1 arg2 ...
should_run() {
    echo "Run: $*"
    echo "---------------"
    "$@" 2>&1 | cat
    local TEST_STATUS=${PIPESTATUS[0]}

    echo ""

    if [ $TEST_STATUS -gt 0 ]; then
        printf "Fail: \'%s\' exited with status %s\n" "$*" "$TEST_STATUS" >&2
        exit 1
    fi
}

# Run command and exit with non-zero if command exits with zero.
# All args interpreted as program to run
# e.g. should_error cmd arg1 arg2 ...
should_error() {
    echo "Run: $*"
    echo "---------------"
    # a shorter alternative is
    # ! "$@" || (echo "failed" && exit 1)

    # this version allow for more formatting
    "$@" 2>&1 | cat  # Exit status of individual commands in a pipeline are ignored
    local TEST_STATUS=${PIPESTATUS[0]} # Get the true status, $? will always be 0

    echo ""

    if [ $TEST_STATUS -eq 0 ]; then
        printf "Fail: \'%s\' should exit with non-zero status\n" "$*" >&2
        exit 1
    fi
}

# Check if (grep) pattern ARG1 is found in string ARG2
# E.g. should_match "foo*" "foobar"
should_match() {
    echo "$2" | grep "$1"  - | cat > /dev/null
    local SEARCH_RES=${PIPESTATUS[1]}

    if [ $SEARCH_RES -gt 0 ]; then
        printf "Fail: Output did not match \'%s\'\n" "$1"
        exit 1
    fi
}
