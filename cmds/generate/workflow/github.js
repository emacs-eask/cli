/**
 * Copyright (C) 2023-2026 the Eask authors.
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

import { e_call } from "../../../src/util.js";

export const command = ['github [file]', 'github-actions [file]', 'gha [file]'];
export const desc = 'Generate the GitHub Actions workflow yaml file';
export const builder = yargs => yargs
  .positional(
    '[file]', {
      description: 'name of the test file; the default is `test.yml`',
      type: 'string',
    });

export const handler = async (argv) => {
  await e_call(argv, 'generate/workflow/github', argv.file);
};
