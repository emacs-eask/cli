---
title: 🔨 Basic Usage
weight: 250
---

Eask’s CLI is fully featured but simple to use, even for those who have very
limited experience working from the command line.

The following is a description of the most common commands you will use while
developing your Eask project. See the [Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
for a comprehensive view of Eask’s CLI.

Once you have installed [Eask](), make sure it is in your `PATH`. You can test
that Eask has been installed correctly via the help command:

```
$ eask --help
```

{{< hint ok >}}
💡 Optionally, you can use `--show-hidden` to show all available commands and
options!
{{< /hint >}}

The output you see in your console should be similar to the following:

```
eask is the main command, used to manage your Emacs dependencies

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp packages.

Usage: eask <command> [options..]

Commands:
  analyze [files..]          Run Eask checker
  archives                   List out all package archives                                                                                                                                     [aliases: sources]
  clean <type>               Delete various files produced during building
  compile [names..]          Byte-compile `.el' files
  create <type>              Create a new elisp project
  docker <version> [args..]  Launch specified Emacs version in a Docker container
  docs [names..]             Build documentation                                                                                                                                                   [aliases: doc]
  emacs [args..]             Execute emacs with the appropriate environment
  eval [form]                Evaluate lisp form with a proper PATH
  path [patterns..]          Print the PATH (exec-path) from workspace                                                                                                                       [aliases: exec-path]
  exec [args..]              Execute command with correct environment PATH set up
  files [patterns..]         Print all package files
  format <type>              Run formatters                                                                                                                                                        [aliases: fmt]
  generate <type>            Generate files that are used for the development
  info                       Display information about the current package
  init [files..]             Initialize project to use Eask
  install-deps               Automatically install package dependencies                                                                                                  [aliases: install-dependencies, prepare]
  install [names..]          Install packages
  keywords                   List available keywords that can be used in the header section
  link <action>              Manage links
  lint <type>                Run linters
  list                       List packages
  load-path [patterns..]     Print the load-path from workspace
  load [files..]             Load elisp files
  outdated                   Show all outdated dependencies
  package-directory          Print path to package directory
  package [destination]      Build a package artifact, and put it into the given destination                                                                                                      [aliases: pack]
  recipe                     Suggest a recipe format
  recompile [names..]        Byte-recompile `.el' files
  refresh                    Download package archives
  reinstall [names..]        Reinstall packages
  run <type>                 Run custom tasks
  search [queries..]         Search packages
  status                     Display the state of the workspace
  test <type>                Run regression/unit tests
  uninstall [names..]        Uninstall packages                                                                                                                                                 [aliases: delete]
  upgrade [names..]          Upgrade packages
  locate                     Print out Eask installed location
  upgrade-eask               Upgrade Eask itself                                                                                                                                          [aliases: upgrade-self]

Proxy Options:
      --proxy        update proxy for HTTP and HTTPS to host                                                                                                                                             [string]
      --http-proxy   update proxy for HTTP to host                                                                                                                                                       [string]
      --https-proxy  update proxy for HTTPS to host                                                                                                                                                      [string]
      --no-proxy     set no-proxy to host                                                                                                                                                                [string]

Options:
      --version      output version information and exit                                                                                                                                                [boolean]
      --help         show usage instructions                                                                                                                                                            [boolean]
      --show-hidden  Show hidden commands and options                                                                                                                                                   [boolean]
  -g, --global       change default workspace to ~/.eask/                                                                                                                                               [boolean]
  -c, --config       change default workspace to ~/.emacs.d/                                                                                                                                            [boolean]
  -a, --all          enable all flag                                                                                                                                                                    [boolean]
  -q, --quick        start cleanly without loading the configuration files                                                                                                                              [boolean]
  -f, --force        enable force flag                                                                                                                                                                  [boolean]
      --debug        turn on debug mode                                                                                                                                                                 [boolean]
      --strict       report error instead of warnings                                                                                                                                                   [boolean]
      --allow-error  continue the executioon even there is error reported                                                                                                                               [boolean]
      --insecure     allow insecure connection                                                                                                                                                          [boolean]
      --no-color     enable/disable color output                                                                                                                                                        [boolean]
  -v, --verbose      set verbosity from 0 to 5                                                                                                                                                           [number]

For more information, find the manual at https://emacs-eask.github.io/
```

## 🗃️ The `eask` Command

The most common usage is probably to run eask with your current directory being
the input directory. Then you run eask followed by a subcommand:

```sh
$ eask info             # Print out Eask-file information
```

Notice the subcommand can be nested:

```sh
$ eask clean workspace  # Deletes your `.eask` folder
```

Pass in option `--help` to look up more information regarding the command you
are using:

```sh
$ eask clean --help
```

The output, and it shows there are 7 subcommands supported:

```
Delete various files produced during building

Usage: eask clean <type> [options..]

Commands:
  clean all                  Do all cleaning tasks                                                                                                                                          [aliases: everything]
  clean autoloads            Remove generated autoloads file
  clean dist [destination]   Delete dist subdirectory                                                                                                                                     [aliases: distribution]
  clean elc                  Remove byte compiled files generated by eask compile
  clean log-file             Remove all generated log files
  clean pkg-file             Remove generated pkg-file
  clean workspace            Clean up .eask directory                                                                                                                                            [aliases: .eask]

Positionals:
  <type>  type of the cleaning task

...
````

Here is a list of known nested subcommands:

- eask create
- eask clean
- eask generate
- eask generate workflow
- eask link
- eask lint
- eask run
- eask source
- eask test

## 📌 Knowing your `elpa` directory

Eask creates an isolated environment, so it won't create any side effects after
playing, testing, and running your elisp packages. But it's important to know
what elpa directory (you can think of this as your `.emacs.d`) the current Eask
session is pointing to, so you can release the full potential of this tool!

Here is how Eask works behind the scene in different scenarios:

| Name   | Description                                                          | Options            | Path         |
|--------|----------------------------------------------------------------------|--------------------|--------------|
| local  | The default behavior, use Eask as package dev tool                   | n/a                | `./.eask`    |
| config | Use Eask as your package manager (It can be used as a test tool too) | `-c` or `--config` | `~/.emacs.d` |
| global | Use Eask as a general tool, it's unrelated to other scopes           | `-g` or `--global` | `~/.eask`    |

You might think of why these rules are created.

It's easy to understand **config** and **local** scopes since many other build
tools use the **local** scope to create an isolated environment. The **config**
scope is an additional feature for people who prefer managing their packages
with an external tool and not by built-in `package.el` or config base
`straight.el`, so you can save up startup time to check if packages are
installed for your Emacs to operate.

So what is the **global** scope in terms of Eask? Why it's needed?

Eask is more than a build tool now. Several commands don't require their
dependencies as package dependencies. For example, the `cat` command:

```sh
$ eask cat [PATTERNS..]
```

`cat` is a simple command that mimics Linux's default `cat` command, but it does
the syntax highlighting for you! How it's implemented? The command relies on
an external package [e2ansi][], and this is neither the `package` nor `config`
dependency (it could be, but let's assume we don't want it).

How do we use this command without side effects to your project or personal
emacs configuration? The global scope is introduced for this problem. Now we
can add any useful commands without worrying your environment got messed
up.

Here is the flowchart describes Eask's lifecycle:

<p align="center">
<img src="images/scopes.png" />
</p>

By default, Eask uses your current directory as your workspace since most of
the time you would just want to operate jobs for your elisp packages.


<!-- Links -->

[e2ansi]: https://github.com/Lindydancer/e2ansi
