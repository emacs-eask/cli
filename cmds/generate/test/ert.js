/**
 * Copyright (C) 2024-2026 the Eask authors.
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

export const command = ['ert [names..]'];
export const desc = 'Create a new test project for the ert tests';
export const builder = yargs => yargs
  .positional(
    '[names..]', {
      description: 'specify test names',
      type: 'array',
    });

export const handler = async (argv) => {
  await e_call(argv, 'generate/test/ert', argv.names);
};
