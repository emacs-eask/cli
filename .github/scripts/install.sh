#!/usr/bin/env bash

echo "$PWD/bin" >> "$GITHUB_PATH"

# See new PATH variable
echo $PATH

# See if `eask` install successfully!
eask --version
