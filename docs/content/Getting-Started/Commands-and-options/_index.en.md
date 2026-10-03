---
title: 🚩 Commands and options
weight: 300
---

{{< toc >}}

The general syntax of the **eask** program is:

```sh
eask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

# 🚩 Creating

## 🔍 eask create package

Create a new elisp project with the default `Eask`-file and CI/CD support.

```sh
eask [GLOBAL-OPTIONS] create package <name>
```

{{< hint info >}}
💡 The template project is located in https://github.com/emacs-eask/template-elisp
{{< /hint >}}

## 🔍 eask create elpa

Create a new ELPA using [github-elpa](https://github.com/10sr/github-elpa).

```sh
eask [GLOBAL-OPTIONS] create elpa <name>
```

{{< hint info >}}
💡 The template project is located in https://github.com/emacs-eask/template-elpa
{{< /hint >}}

## 🔍 eask create el-project

Create a new project with [el-project](https://github.com/Kyure-A/el-project).

```sh
eask [GLOBAL-OPTIONS] create el-project
```

# 🚩 Core

Often use commands that are uncategorized.

## 🔍 eask init

Initialize the current directory to start using Eask.

```sh
eask [GLOBAL-OPTIONS] init
```

Eask will generate the file like this:

```elisp
(package "PACKAGE-NAME"
         "VERSION"
         "YOUR PACKAGE SUMMARY")

(website-url "https://example.com/project-url/")
(keywords "KEYWORD1" "KEYWORD2")

(package-file "PACKAGE-FILE")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(depends-on "emacs" "26.1")
```

**[RECOMMENDED]** If you already have an elisp project, you can convert the
`.el` file to Eask-file:

```sh
eask init --from source /path/to/source.el
```

If you already have a [Cask][] project, you can convert Cask-file to Eask-file:

```sh
eask init --from cask /path/to/Cask
```

If you already have a [Eldev][] project, you can convert Eldev-file to Eask-file:

```sh
eask init --from eldev /path/to/Eldev
```

If you already have a [Keg][] project, you can convert Keg-file to Eask-file:

```sh
eask init --from keg /path/to/Keg
```

{{< hint ok >}}
💡 See section [Examples](https://emacs-eask.github.io/examples) for more
Eask-file examples!
{{< /hint >}}

## 🔍 eask info

Show information about the project or configuration.

```sh
eask [GLOBAL-OPTIONS] info
```

## 🔍 eask status

Show the workspace status.

```sh
eask [GLOBAL-OPTIONS] status
```

## 🔍 eask install

To install packages from archives or install from the workspace.

```sh
eask [GLOBAL-OPTIONS] install [PACKAGES..]
```

Install packages by specifying arguments:

```sh
eask install auto-complete helm magit
```

Or else, it will install the package from the current development:

```sh
eask install
```

## 🔍 eask install-deps

To install all dependencies.

Alias: `install-dependencies`, `prepare`

```sh
eask [GLOBAL-OPTIONS] install-deps [--dev]
```

{{< hint ok >}}
💡 Specify option [--dev] to install dependencies from the development scope.
{{< /hint >}}

## 🔍 eask install-file

Install packages from files, `.tar` files, or directories.

```sh
eask [GLOBAL-OPTIONS] install-file [FILES..]
```

## 🔍 eask install-vc

Fetch and install packages directly via version control.

```sh
eask [GLOBAL-OPTIONS] install-vc [SPECS..]
```

## 🔍 eask uninstall

To uninstall/delete packages.

```sh
eask [GLOBAL-OPTIONS] uninstall [PACKAGES..]
```

Uninstall packages by specifying arguments:

```sh
eask uninstall dash f s
```

Or else, it will uninstall the package from the current workspace:

```sh
eask uninstall
```

## 🔍 eask reinstall

To reinstall packages from archives.

```sh
eask [GLOBAL-OPTIONS] reinstall [PACKAGES..]
```

## 🔍 eask package

Build the package artifact.

Alias: `pack`

```sh
eask package [DESTINATION]
```

If [DESTINATION] is not specified, it will generate to the `/dist` folder
by default.

## 🔍 eask compile

Byte-compile `.el` files.

```sh
eask compile [FILES..]
```

Compile files by specifying arguments:

```sh
eask compile file-1.el file-2.el
```

Or compile files that are already specified in your `Eask`-file.

```sh
eask compile
```

## 🔍 eask recompile

Byte-recompile `.el` files.

```sh
eask recompile [FILES..]
```

{{< hint info >}}
💡 Similar to `eask compile`, but it will also remove old `.elc` files before
compiling.
{{< /hint >}}

## 🔍 eask package-directory

Print the path to package directory, where all dependencies are installed.

```sh
eask [GLOBAL-OPTIONS] package-directory
```

## 🔍 eask path

Print the `PATH` environment variable of this project.

Alias: `exec-path`

```sh
eask [GLOBAL-OPTIONS] path [PATTERNS..]
```

Optionally, you can pass in `[PATTERNS..]` to perform the search.

## 🔍 eask load-path

Print the load path containing the dependencies of the current project.

```sh
eask [GLOBAL-OPTIONS] load-path [PATTERNS..]
```

Optionally, you can pass in `[PATTERNS..]` to perform the search.

## 🔍 eask files

Print the list of all package files.

```sh
eask [GLOBAL-OPTIONS] files [PATTERNS..]
```

If `[PATTERNS..]` are defined, it will display files that match that pattern.

## 🔍 eask recipe

Suggest a recipe format.

```sh
eask [GLOBAL-OPTIONS] recipe [FILES..]
```

## 🔍 eask keywords

Display the available keywords for use in the header section.

```sh
eask [GLOBAL-OPTIONS] keywords
```

## 🔍 eask bump

Bump version for your project and/or Eask-file.

```sh
eask [GLOBAL-OPTIONS] bump [LEVELS..]
```

{{< hint info >}}
💡 Argument **[LEVELS..]** accepts **major**, **minor** and/or **patch**!
{{< /hint >}}

## 🔍 eask cat

View filename(s).

The positional argument `[PATTERNS..]` is an array of wildcard patterns.

```sh
eask [GLOBAL-OPTIONS] cat [PATTERNS..]
```

{{< hint info >}}
💡 This command uses the package [e2ansi](https://github.com/Lindydancer/e2ansi)
to accomplish the syntax highlighting.
{{< /hint >}}

## 🔍 eask concat

Concatenate all Emacs Lisp files into one file.

```sh
eask [GLOBAL-OPTIONS] concat [FILES..]
```

## 🔍 eask loc

Print LOC information.

```sh
eask [GLOBAL-OPTIONS] loc [FILES..]
```

# 🚩 Documentation

Commands used to build documentation site.

## 🔍 eask docs el2org

Build documentation with [el2org][].

```sh
eask [GLOBAL-OPTIONS] docs el2org [NAMES..]
```

# 🚩 Execution

Commands allow you to execute on top of the Eask core.

Basically, this allows you to do anything you want!

## 🔍 eask load

Load Emacs Lisp files in order.

```sh
eask [GLOBAL-OPTIONS] load [FILES..]
```

## 🔍 eask exec

Execute the system command with the given arguments.

```sh
eask [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]
```

## 🔍 eask emacs

Execute emacs with the appropriate environment.

```sh
eask [GLOBAL-OPTIONS] emacs [ARGUMENTS ...]
```

## 🔍 eask eval

Evaluate `FORM` as a lisp form.

```sh
eask [GLOBAL-OPTIONS] eval [FORM]
```

## 🔍 eask repl

Start an Elisp REPL.

```sh
eask [GLOBAL-OPTIONS] repl [FILES..]
```

Alias: `ielm`

## 🔍 eask run script

Run the script.

```sh
eask [GLOBAL-OPTIONS] run script [NAMES..]
```

## 🔍 eask run command

Run the command.

Alias: `cmd`

```sh
eask [GLOBAL-OPTIONS] run command [NAMES..]
```

## 🔍 eask docker

Launch specified Emacs version in a Docker container.

```sh
eask [GLOBAL-OPTIONS] docker <VERSION> [ARGUMENTS ...]
```

For example:

```sh
eask docker 26.1 info
```

This is the same as jumping right into Emacs 26.1 (in docker) and executing
`eask info`.

# 🚩 Management

Commands that help you manage your package's dependencies.

## 🔍 eask archives

List out all package archives.

```sh
eask [GLOBAL-OPTIONS] archives
```

## 🔍 eask search

Search packages from archives.

```sh
eask [GLOBAL-OPTIONS] search [QUEIRES..]
```

## 🔍 eask upgrade

Upgrade all packages from archives.

```sh
eask [GLOBAL-OPTIONS] upgrade
```

## 🔍 eask list

List all installed packages in dependency tree form.

```sh
eask [GLOBAL-OPTIONS] list [--depth]
```

## 🔍 eask outdated

List out all outdated packages.

```sh
eask [GLOBAL-OPTIONS] outdated [--depth]
```

## 🔍 eask refresh

Download descriptions of all configured package archives.

```sh
eask [GLOBAL-OPTIONS] refresh
```

# 🚩 Generating

Generate files that are used for the development.

## 🔍 eask generate autoloads

Generate the autoload file.

Write a package autoloads to `project-autoloads.el` in the project root.

```sh
eask [GLOBAL-OPTIONS] generate autoloads
```

`project` is the project name, as declared in `Eask`-file. See
[Multi-file Packages (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)
for details.

## 🔍 eask generate pkg-file

Generate the pkg file.

Write a package descriptor file to `project-pkg.el` in the project root.

Alias: `pkg`, `pkg-el`

```sh
eask [GLOBAL-OPTIONS] generate pkg-file
```

`project` is the project name, as declared in `Eask`-file. See
[Multi-file Packages (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)
for details.

## 🔍 eask generate recipe

Generate the recipe file.

```sh
eask [GLOBAL-OPTIONS] generate recipe [DESTINATION]
```

If [DESTINATION] is not specified, it will generate to the `/recipes` folder
by default.

## 🔍 eask generate license

Generate a LICENSE file.

```sh
eask [GLOBAL-OPTIONS] generate license <name>
```

`name` is the type of the license, see https://api.github.com/licenses for all
the choices.

{{< hint info >}}
💡 This command uses the package [license-templates](https://github.com/jcs-elpa/license-templates)
to generate ignore file.
{{< /hint >}}

## 🔍 eask generate ignore

Generate an ignore file.

```sh
eask [GLOBAL-OPTIONS] generate ignore <name>
```

{{< hint info >}}
💡 This command uses the package [gitignore-templates](https://github.com/xuchunyang/gitignore-templates.el)
to generate ignore file.
{{< /hint >}}

## 🔍 eask generate test ert

Create a new test project for the [ert][] tests.

```sh
eask [GLOBAL-OPTIONS] generate test ert [NAMES..]
```

## 🔍 eask generate test ert-runner

Create a new test project for the [ert-runner][].

```sh
eask [GLOBAL-OPTIONS] generate test ert-runner [NAMES..]
```

## 🔍 eask generate test buttercup

Create a new [Buttercup][] setup for the project.

```sh
eask [GLOBAL-OPTIONS] generate test buttercup
```

## 🔍 eask generate test ecukes

Create a new [Ecukes][] setup for the project.

```sh
eask [GLOBAL-OPTIONS] generate test ecukes
```

## 🔍 eask generate workflow circle-ci

Generate the [CircleCI][] workflow yaml file.

The default filename is `config.yml`.

```sh
eask [GLOBAL-OPTIONS] generate workflow circle-ci [--file]
```

This will generate the yaml file under `.circleci/`!

## 🔍 eask generate workflow github

Generate the [GitHub Actions][] workflow yaml file.

The default filename is `test.yml`.

```sh
eask [GLOBAL-OPTIONS] generate workflow github [--file]
```

This will generate the yaml file under `.github/workflow/`!

## 🔍 eask generate workflow gitlab

Generate the [GitLab Runner][] workflow yaml file.

The default filename is `.gitlab-ci.yml`.

```sh
eask [GLOBAL-OPTIONS] generate workflow gitlab [--file]
```

## 🔍 eask generate workflow travis-ci

Generate the [Travis CI][] workflow yaml file.

The default filename is `.travis.yml`.

```sh
eask [GLOBAL-OPTIONS] generate workflow travis-ci [--file]
```

# 🚩 Linking

Link between this package and a dependency on the local filesystem. A linked
dependency avoids the need to download a dependency from a remote archive. The
package linked to must either have a `Eask`-file or a `-pkg.el`-file.

## 🔍 eask link add

Links the given *source* directory into the package directory of this project,
under the given *package* name.

```sh
eask [GLOBAL-OPTIONS] link add <NAME> <PATH>
```

## 🔍 eask link delete

Delete locally linked packages.

Alias: `remove`

```sh
eask [GLOBAL-OPTIONS] link delete [NAMES..]
```

## 🔍 eask link list

List all links.

```sh
eask [GLOBAL-OPTIONS] link list
```

# 🚩 Cleaning

Delete various files produced during building.

## 🔍 eask clean workspace

Delete the `.eask` from the current workspace.

Alias: `.eask`

```sh
eask [GLOBAL-OPTIONS] clean workspace
```

⛔️ Don't specify the option `--config, -c`, or else it will delete your entire `~/.emacs.d`.

```elisp
eask clean workspace -g
```

## 🔍 eask clean elc

Delete all `.elc` files. This would respect to your `Eask` file.

```sh
eask [GLOBAL-OPTIONS] clean elc
```

## 🔍 eask clean dist

Delete the `dist` directory where the build output is stored.

Alias: `distribution`

```sh
eask [GLOBAL-OPTIONS] clean dist
```

## 🔍 eask clean autoloads

Remove the generated autoloads file.

```sh
eask [GLOBAL-OPTIONS] clean autoloads
```

## 🔍 eask clean pkg-file

Remove the generated pkg-file.

```sh
eask [GLOBAL-OPTIONS] clean pkg-file
```

## 🔍 eask clean log-file

Remove all generated log files.

```sh
eask [GLOBAL-OPTIONS] clean log-file
```

## 🔍 eask clean all

This command is the combination of all other clean commands.

- `clean workspace`
- `clean elc`
- `clean dist`
- `clean autoloads`
- `clean pkg-file`
- `clean log-file`

Alias: `everything`

```sh
eask [GLOBAL-OPTIONS] clean all
```

# 🚩 Linting

Commands that lint your Emacs package.

## 🔍 eask lint package

Run [package-lint](https://github.com/purcell/package-lint).

```sh
eask [GLOBAL-OPTIONS] lint package [FILES..]
```

## 🔍 eask lint checkdoc

Run checkdoc (built-in).

```sh
eask [GLOBAL-OPTIONS] lint checkdoc [FILES..]
```

## 🔍 eask lint elint

Run elint (built-in).

```sh
eask [GLOBAL-OPTIONS] lint elint [FILES..]
```

## 🔍 eask lint elisp-lint

Run [elisp-lint](https://github.com/gonewest818/elisp-lint).

```sh
eask [GLOBAL-OPTIONS] lint elisp-lint [FILES..]
```

This does respect the `.dir-locals.el` file! 🎉

## 🔍 eask lint elsa

Run [elsa](https://github.com/emacs-elsa/Elsa).

```sh
eask [GLOBAL-OPTIONS] lint lint elsa [FILES..]
```

## 🔍 eask lint indent

Run indent-lint.

```sh
eask [GLOBAL-OPTIONS] lint indent [FILES..]
```

## 🔍 eask lint declare

Run check-declare (built-in).

```sh
eask [GLOBAL-OPTIONS] lint declare [FILES..]
```

## 🔍 eask lint regexps

Run [relint](https://github.com/mattiase/relint).

Alias: `lint relint`

```sh
eask [GLOBAL-OPTIONS] lint regexps [FILES..]
```

## 🔍 eask lint keywords

Run keywords checker (built-in).

```sh
eask [GLOBAL-OPTIONS] lint keywords
```

## 🔍 eask lint license

Run license check.

```sh
eask [GLOBAL-OPTIONS] lint license
```

## 🔍 eask lint org

Run `org-lint` on Org files.

```sh
eask [GLOBAL-OPTIONS] lint org [FILES..]
```

# 🚩 Testing

Run regression/unit tests.

## 🔍 eask test activate

Activate package; use to test the package activation

```sh
eask [GLOBAL-OPTIONS] activate [FILES..]
```

{{< hint info >}}
💡 You can pass in **[FILES..]** so you can test your package activation fully!

**[FILES..]** will be loaded after the package is activated.
{{< /hint >}}

## 🔍 eask test ert

Run [ert][] tests.

```sh
eask [GLOBAL-OPTIONS] test ert [FILES..]
```

## 🔍 eask test ert-runner

Run [ert][] test using [ert-runner][].

```sh
eask [GLOBAL-OPTIONS] test ert-runner [FILES..] [-p PATTERN] [-t TAGS] [--reporter NAME]
```

{{< hint info >}}
💡 You can pass in **-p, --pattern** so only the tests matching the pattern are executed!

```sh
eask test ert-runner -p some-specific-test
```

💡 You can pass in **-t, --tags** so only the tests carrying the given tags are executed!

💡 You can pass in **--reporter** to change the reporter to use; e.g. `--reporter ert`.
{{< /hint >}}

## 🔍 eask test buttercup

Run [buttercup][] tests.

```sh
eask [GLOBAL-OPTIONS] test buttercup
```

## 🔍 eask test ecukes

Run [ecukes][] tests.

```sh
eask [GLOBAL-OPTIONS] test ecukes [FILES..]
```

## 🔍 eask test melpazoid

Run [melpazoid][] tests.

```sh
eask [GLOBAL-OPTIONS] test melpazoid [DIRECTORIES..]
```

{{< hint info >}}
💡 If **[DIRECTORIES..]** is not passed in; it will use the current workspace instead.
{{< /hint >}}

# 🚩 Formatting

Commands that formats your Emacs source files.

## 🔍 eask format elisp-autofmt

Run [elisp-autofmt][] formatter.

```sh
eask [GLOBAL-OPTIONS] format elisp-autofmt [FILES..]
```

## 🔍 eask format elfmt

Run [elfmt][] formatter.

```sh
eask [GLOBAL-OPTIONS] format elfmt [FILES..]
```

# 🚩 Control DSL

List of commands that control DSL.

## 🔍 eask source add

Add an archive source.

```sh
eask [GLOBAL-OPTIONS] source add <NAME> [URL]
```

## 🔍 eask source delete

Remove an archive source.

Alias: `remove`

```sh
eask [GLOBAL-OPTIONS] source delete <NAME>
```

## 🔍 eask source list

List all source information.

```sh
eask [GLOBAL-OPTIONS] source list
```

{{< hint info >}}
💡 This command is the same as `eask archives`!
{{< /hint >}}

# 🚩 Utilities

Other helper commands.

## 🔍 eask upgrade-eask

Upgrade Eask to the latest version.

Alias: `upgrade-self`

```sh
eask [GLOBAL-OPTIONS] upgrade-eask
```

{{< hint warning >}}
💡 This will only work if you install it from the source!
{{< /hint >}}

## 🔍 eask locate

Show the location where Eask is installed.

```sh
eask [GLOBAL-OPTIONS] locate
```

## 🔍 eask root

Display the effective installation directory of your Emacs packages.

```sh
eask [GLOBAL-OPTIONS] root
```

# 🚩 Checker

Commands to check your Eask-file.

## 🔍 eask analyze

Lint an `Eask`-file.

```sh
eask [GLOBAL-OPTIONS] analyze [FILES..]
```

Example:

```bash
# lint all Eask-files in the current directory and subdirectories
eask analyze
# lint specific files
eask analyze Eask Eask.27
# lint all Eask-files in specified directory and subdirectories
eask analyze src/
# print result as JSON
eask analyze --json
```

For more detail, run `eask analyze --help`.

# 🚩 Global Options

The following options are available on all Eask commands:

## 🔍 --global, -g

This will use `~/.eask/` instead of the package development environment.

This is used for other tasks. e.g., `cat`, etc.

```sh
eask -g [COMMAND]
```

## 🔍 --config, -c

This will use `~/.emacs.d/` instead of the package development environment.

This is used for doing stuff for your **Emacs configuration**. e.g., package
management, etc.

```sh
eask -c [COMMAND]
```

## 🔍 --all, -a

Enable the `all` flag.

```sh
eask -a [COMMAND]
```

## 🔍 --quick, -q

Start cleanly without loading the configuration files.

```sh
eask -q [COMMAND]
```

## 🔍 --force, -f

Force command's execution.

Force to uninstall the package `dash` even it's a dependency from another packages.

```sh
eask -f [COMMAND]
```

## 🔍 --debug

Enable debug information.

This is equivalent to:

```elisp
(setq debug-on-error t)
```

## 🔍 --strict

Trigger error instead of warnings.

For instance, in **eask compile**:

```elisp
(setq byte-compile-error-on-warn t)
```

## 🔍 --allow-error

Continue the execution without killing the Emacs.

## 🔍 --insecure

Connect archives with HTTP instead of HTTPS.

## 🔍 --timestamps

Enable/Disable timestamps.

## 🔍 --log-level

Enable/Disable log header.

## 🔍 --log-file, --lf

Weather to generate log files.

## 🔍 --no-color

Disable color output.

## 🔍 --elapsed-time, --et

Show elapsed time between each operation.

## 🔍 --verbose, -v `<integer>`

Set verbosity from 0 to 5.

```sh
eask --verbose 4 [COMMAND]
```

## 🔍 --version

Show version number.

## 🔍 --help

Show help.

# 🚩 Proxy Options

## 🔍 --proxy `<proxy>`

Set Emacs proxy for HTTP and HTTPS:

```sh
eask --proxy "localhost:8888" [COMMAND]
```

## 🔍 --http-proxy `<proxy>`

Set Emacs proxy for HTTP only.

## 🔍 --https-proxy `<proxy>`

Set Emacs proxy for HTTPS only.

## 🔍 --no-proxy `<pattern>`

Do not use a proxy for any URL matching pattern.

`<pattern>`is an Emacs regular expression.


<!-- Links -->

[Cask]: https://github.com/cask/cask
[Eldev]: https://emacs-eldev.github.io/eldev/
[Keg]: https://github.com/conao3/keg.el

[CircleCI]: https://circleci.com/
[GitHub Actions]: https://github.com/features/actions
[GitLab Runner]: https://docs.gitlab.com/runner/
[Travis CI]: https://www.travis-ci.com/

[el2org]: https://github.com/tumashu/el2org

[ert]: https://www.gnu.org/software/emacs/manual/html_node/ert/
[ert-runner]: https://github.com/rejeep/ert-runner.el
[buttercup]: https://github.com/jorgenschaefer/emacs-buttercup
[ecukes]: https://github.com/ecukes/ecukes
[melpazoid]: https://github.com/riscy/melpazoid

[elisp-autofmt]: https://codeberg.org/ideasman42/emacs-elisp-autofmt
[elfmt]: https://github.com/riscy/elfmt
