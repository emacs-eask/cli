/**
 * Copyright (C) 2022-2023 Jen-Chieh Shen
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
 * along with GNU Emacs; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

"use strict";

import * as os from 'https://deno.land/std@0.170.0/node/os.ts';
import * as fs from 'https://deno.land/std@0.152.0/node/fs/promises.ts';
import * as path from 'https://deno.land/std@0.142.0/path/mod.ts';
import * as process from 'https://deno.land/std@0.104.0/node/process.ts';

const __filename = path.fromFileUrl(import.meta.url);
const __dirname = path.dirname(path.fromFileUrl(import.meta.url));

const IS_PKG = path.basename(Deno.execPath()).startsWith('eask');

const GITHUB_ACTIONS = (process.env.GITHUB_WORKSPACE !== undefined);

const EASK_HOMEDIR = os.homedir()?.replace(/\\/g, '/') + '/.eask/';

export {
    __filename,
    __dirname,
    os,
    fs,
    path,
    process,
    IS_PKG,
    GITHUB_ACTIONS,
    EASK_HOMEDIR,
};
