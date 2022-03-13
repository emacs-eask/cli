#!/usr/bin/env node

const yargs = require("yargs");

yargs
  .usage("$0 <cmd> [args]")
  .commandDir("cmds")
  .demandCommand()
  .help().argv;
