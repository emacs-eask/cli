#!/bin/sh
# ========================================================================
# $File: commands.sh $
# $Date: 2022-03-13 21:44:55 $
# $Revision: $
# $Creator: Jen-Chieh Shen $
# $Notice: See LICENSE.txt for modification and distribution information
#                   Copyright © 2022 by Shen, Jen-Chieh $
# ========================================================================

# Following command is commented due to CI
#
#cd ..

npm install

CMD="node index.js"

echo "[INFO] 'eask' version: "
$CMD --version
