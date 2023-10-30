/**
 * Copyright (C) 2023 Jen-Chieh Shen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

"use strict";

exports.command = ['gitlab [file]', 'gitlab-runner [file]'];
exports.desc = 'Generate GitLab Runner workflow yaml file';
exports.builder = yargs => yargs
  .positional(
    '[file]', {
      description: 'name of the test file; the default is `.gitlab-ci.yml`',
      type: 'string',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'generate/workflow/gitlab', argv.file);
};

