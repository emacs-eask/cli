/**
 * $File: install.js $
 * $Date: 2022-03-13 21:31:28 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const { exec } = require("child_process");

exports.command = "delete-pass [service] [account]";
exports.desc = "Delete the stored password for the `service` and `account`";
exports.builder = {
  package: {
    alias: "s",
    requiresArg: true
  },
  global: {
    alias: "g",
    requiresArg: false
  }
};

exports.handler = async ({ package, global }) => {
  if (!package || !global) {
    throw new Error("provide required params (package, global)");
  }

  await exec("emacs --script install.el " + package, (error, stdout, stderr) => {
    console.log(stdout);
  });
};
