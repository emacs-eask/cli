---
title: 🔨 使用基礎
weight: 250
---

Eask 的 CLI 功能齊全但易於使用，即使對於那些使用命令行的經驗非常有限的人也是如此。

以下是您在開發 Eask 項目時將使用的最常用命令的說明。 請參閱
[命令和選項](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
以全面了解 Eask 的 CLI。

一旦你安裝了 [Eask]()，確保它在你的 `PATH` 中。 您可以通過 help 命令測試 Eask 是否已正確安裝：

```
$ eask --help
```

{{< hint ok >}}
💡 或者，您可以使用 `--show-hidden` 來顯示所有可用的命令和選項！
{{< /hint >}}

您在控制台中看到的輸出應類似於以下內容：

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

## 🗃️ `eask` 命令

最常見的用法可能是在當前目錄作為輸入目錄的情況下運行 eask。 然後你運行 eask 後跟一個子命令：

```sh
$ eask info             # 打印出Eask文件信息
```

Notice the subcommand can be nested:

```sh
$ eask clean workspace  # 刪除你的 .eask 文件夾
```

傳遞選項 `--help` 以查找有關您正在使用的命令的更多信息：

```sh
$ eask clean --help
```

輸出，它顯示支持 7 個子命令：

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

以下是已知的嵌套子命令列表：

- eask create
- eask clean
- eask generate
- eask generate workflow
- eask link
- eask lint
- eask run
- eask source
- eask test

## 📌 了解你的 `elpa` 目錄

Eask 創建了一個隔離的環境，因此在播放、測試和運行您的 elisp 包後它不會產生任何副作用。
但了解當前 Eask 會話指向的 elpa 目錄（您可以將其視為您的 `.emacs.d`）非常重要，這樣您
才能釋放該工具的全部潛力！

以下是 Eask 在不同場景下的幕後工作方式：

| 名稱   | 描述                                              | 選項               | 路徑         |
|--------|---------------------------------------------------|--------------------|--------------|
| local  | 默認行為，使用 Eask 作為包開發工具                | n/a                | `./.eask`    |
| config | 使用 Eask 作為您的包管理器 (它也可以用作測試工具) | `-c` or `--config` | `~/.emacs.d` |
| global | Eask 作為通用工具使用，與其他範圍無關             | `-g` or `--global` | `~/.eask`    |

您可能會想到為什麼要創建這些規則。

**config** 和 **local** 範圍很容易理解，因為許多其他構建工具使用 **local** 範圍來創建隔離環境。 **config**
範圍是一項附加功能，適用於喜歡使用外部工具而不是內置 `package.el` 或配置基礎 `straight.el` 管理包的人，
因此您可以節省啟動時間 檢查是否為您的 Emacs 運行安裝了軟件包。

那麼 Eask 的 **global** 範圍是什麼？ 為什麼需要它？

Eask 現在不僅僅是一個構建工具。 一些命令不需要它們的依賴項作為包依賴項。 例如，`cat` 命令：

```sh
$ eask cat [PATTERNS..]
```

`cat` 是一個模仿 Linux 的默認 `cat` 命令的簡單命令，但它會為您突出顯示語法！ 它是如何實施的？
該命令依賴於外部包 [e2ansi][]，這既不是 `package` 也不是 `config` 依賴項（它可能是，但假設我們不需要它）。

我們如何使用這個命令而不會對您的項目或個人 emacs 配置產生副作用？ 針對這個問題引入了全局範圍。
現在我們可以添加任何有用的命令而不用擔心你的環境被搞砸了。

下面是描述 Eask 生命週期的流程圖：

<p align="center">
<img src="images/scopes.png" />
</p>

默認情況下，Eask 使用您的當前目錄作為您的工作區，因為大多數時候您只想為您的 elisp 包運行作業。


<!-- Links -->

[e2ansi]: https://github.com/Lindydancer/e2ansi
