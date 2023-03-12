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
$ eask help
```

The output you see in your console should be similar to the following:

```
eask is the main command, used to manage your Emacs dependencies

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp packages.

Usage: eask <command> [options..]

Commands:
  archives                List out all package archives                                                                                                                                        [aliases: sources]
  cat <patterns>          View filename(s)                                                                                                                                                        [aliases: type]
  clean <type>            Delete various files produced during building
  compile [names..]       Byte compile all Emacs Lisp files in the package
  concat [names..]        Concatenate elisp files                                                                                                                                          [aliases: concatenate]
  create <type>           Create a new elisp project
  emacs [args..]          Execute emacs with the appropriate environment
  eval [form]             Evaluate lisp form with a proper PATH
  path [patterns..]       Print the PATH (exec-path) from workspace                                                                                                                          [aliases: exec-path]
  exec [args..]           Execute command with correct environment PATH set up
  files [patterns..]      Print all package files
  generate <type>         Generate files that are used for the development
  info                    Display information about the current package
  init [files..]          Initialize project to use Eask
  install-deps            Automatically install package dependencies                                                                                                     [aliases: install-dependencies, prepare]
  install [names..]       Install packages
  keywords                List available keywords that can be used in the header section
  link <action>           Manage links
  lint <type>             Run linter
  list                    List packages
  load-path [patterns..]  Print the load-path from workspace
  load [files..]          Load elisp files
  outdated                Show all outdated dependencies
  package-directory       Print path to package directory
  package [destination]   Build a package artifact, and put it into the given destination
  recipe                  Suggest a recipe format
  refresh                 Download package archives
  reinstall [names..]     Reinstall packages
  run [names..]           Run the script named [names..]                                                                                                                                    [aliases: run-script]
  search [queries..]      Search packages
  test <type>             Run test
  uninstall [names..]     Uninstall packages                                                                                                                                                    [aliases: delete]
  upgrade [names..]       Upgrade packages
  check-eask [files..]    Run eask checker
  locate                  Print out Eask installed location
  upgrade-eask            Upgrade Eask itself                                                                                                                                             [aliases: upgrade-self]

Proxy Options:
      --proxy        update proxy for HTTP and HTTPS to host                                                                                                                                             [string]
      --http-proxy   update proxy for HTTP to host                                                                                                                                                       [string]
      --https-proxy  update proxy for HTTPS to host                                                                                                                                                      [string]
      --no-proxy     set no-proxy to host                                                                                                                                                                [string]

Options:
      --version             show version number                                                                                                                                                         [boolean]
      --help                show usage instructions                                                                                                                                                     [boolean]
  -g, --global              change default workspace to ~/.emacs.d/                                                                                                                                     [boolean]
  -a, --all                 enable all flag                                                                                                                                                             [boolean]
  -q, --quick               start cleanly without loading the configuration files                                                                                                                       [boolean]
  -f, --force               enable force flag                                                                                                                                                           [boolean]
      --development, --dev  turn on development mode                                                                                                                                                    [boolean]
      --debug               turn on debug mode                                                                                                                                                          [boolean]
      --strict              report error instead of warnings                                                                                                                                            [boolean]
      --allow-error         continue the executioon even there is error reported                                                                                                                        [boolean]
      --insecure            allow insecure connection                                                                                                                                                   [boolean]
      --timestamps          log with timestamps                                                                                                                                                         [boolean]
      --log-level           log with level                                                                                                                                                              [boolean]
      --log-file, --lf      generate log files                                                                                                                                                          [boolean]
      --elapsed-time, --et  show elapsed time between each operation                                                                                                                                    [boolean]
      --no-color            disable color output                                                                                                                                                        [boolean]
  -v, --verbose             set verbosity from 0 to 4                                                                                                                                                    [number]

For more information, find the manual at https://emacs-eask.github.io/
```

## The `eask` Command

The most common usage is probably to run eask with your current directory being
the input directory.
