---
title: 🚩 命令和選項
weight: 300
---

{{< toc >}}

**eask** 程序的一般語法是：

```sh
eask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

# 🚩 創建

## 🔍 eask create package

使用默認的 `Eask` 文件和 CI/CD 支持創建一個新的 elisp 項目。

```sh
eask [GLOBAL-OPTIONS] create package <name>
```

{{< hint info >}}
💡 模板項目位於 https://github.com/emacs-eask/template-elisp。
{{< /hint >}}

## 🔍 eask create elpa

使用 [github-elpa](https://github.com/10sr/github-elpa) 創建一個新的 ELPA。

```sh
eask [GLOBAL-OPTIONS] create elpa <name>
```

{{< hint info >}}
💡 模板項目位於 https://github.com/emacs-eask/template-elpa。
{{< /hint >}}

## 🔍 eask create el-project

使用 [el-project](https://github.com/Kyure-A/el-project) 創建一個新專案。

```sh
eask [GLOBAL-OPTIONS] create el-project
```

# 🚩 核心

經常使用未分類的命令。

## 🔍 eask init

初始化當前目錄以開始使用 Eask。

```sh
eask [GLOBAL-OPTIONS] init
```

Eask 將生成這樣的文件：

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

**[推薦]** 如果您已有 elisp 項目，您可以將 `.el` 文件轉換為 Eask 文件：

```sh
eask init --from source /path/to/source.el
```

如果您已有 [Cask][] 項目，您可以將 Cask 文件轉換為 Eask 文件：

```sh
eask init --from cask /path/to/Cask
```

如果您已有 [Eldev][] 項目，您可以將 Eldev 文件轉換為 Eask 文件：

```sh
eask init --from eldev /path/to/Eldev
```

如果您已有 [Keg][] 項目，您可以將 Keg 文件轉換為 Eask 文件：

```sh
eask init --from keg /path/to/Keg
```

{{< hint ok >}}
💡 有關更多 Eask 文件示例，請參閱[示例](https://emacs-esk.github.io/examples)部分！
{{< /hint >}}

## 🔍 eask info

顯示有關項目或配置的信息。

```sh
eask [GLOBAL-OPTIONS] info
```

## 🔍 eask status

顯示現在 workspace 的狀態。

```sh
eask [GLOBAL-OPTIONS] status
```

## 🔍 eask install

從包源或 workspace 安裝包。

```sh
eask [GLOBAL-OPTIONS] install [PACKAGES..]
```

通過指定參數安裝包：

```sh
eask install auto-complete helm magit
```

否則，它將安裝當前開發的包：

```sh
eask install
```

## 🔍 eask install-deps

安裝所有依賴項。

別名: `install-dependencies`, `prepare`

```sh
eask [GLOBAL-OPTIONS] install-deps [--dev]
```

{{< hint ok >}}
💡 指定選項 [--dev] 從開發範圍安裝依賴項。
{{< /hint >}}

## 🔍 eask install-file

從檔案、`.tar` 檔案或目錄安裝套件。

```sh
eask [GLOBAL-OPTIONS] install-file [FILES..]
```

## 🔍 eask install-vc

直接透過版本控制擷取及安裝套件。

```sh
eask [GLOBAL-OPTIONS] install-vc [SPECS..]
```

## 🔍 eask uninstall

卸載/刪除包。

```sh
eask [GLOBAL-OPTIONS] uninstall [PACKAGES..]
```

通過指定參數卸載軟件包：

```sh
eask uninstall dash f s
```

否則，它將從當前開發中卸載包：

```sh
eask uninstall
```

## 🔍 eask reinstall

重新安裝包.

```sh
eask [GLOBAL-OPTIONS] reinstall [PACKAGES..]
```

## 🔍 eask package

構建包工件。

別名: `pack`

```sh
eask package [DESTINATION]
```

如果未指定 [DESTINATION]，則默認導出到 `/dist` 文件夾。

## 🔍 eask compile

字節編譯 `.el` 文件。

```sh
eask compile [FILES..]
```

通過指定參數編譯文件：

```sh
eask compile file-1.el file-2.el
```

或者編譯已經在你的 `Eask` 文件中指定的文件。

```sh
eask compile
```

## 🔍 eask recompile

重新字節編譯 `.el` 文件。

```sh
eask recompile [FILES..]
```

{{< hint info >}}
💡 與 `eask compile` 類似，但它在編譯前會刪除舊的 `.elc` 檔案。
{{< /hint >}}

## 🔍 eask package-directory

打印包目錄的路徑，其中安裝了所有依賴項。

```sh
eask [GLOBAL-OPTIONS] package-directory
```

## 🔍 eask path

打印此項目的 `PATH` 環境變量。

別名: `exec-path`

```sh
eask [GLOBAL-OPTIONS] path [PATTERNS..]
```

或者，您可以傳入 `[PATTERNS..]` 來執行搜索。

## 🔍 eask load-path

打印包含當前項目依賴項的加載路徑。

```sh
eask [GLOBAL-OPTIONS] load-path [PATTERNS..]
```

或者，您可以傳入 `[PATTERNS..]` 來執行搜索。

## 🔍 eask files

打印所有包文件的列表。

```sh
eask [GLOBAL-OPTIONS] files [PATTERNS..]
```

如果定義了 `[PATTERNS..]` ，它將顯示與該模式匹配的文件。

## 🔍 eask recipe

建議 recipe 格式。

```sh
eask [GLOBAL-OPTIONS] recipe [FILES..]
```

## 🔍 eask keywords

顯示可在標頭部分使用的關鍵字。

```sh
eask [GLOBAL-OPTIONS] keywords
```

## 🔍 eask bump

為你的專案或 Eask-file 遞增版本號。

```sh
eask [GLOBAL-OPTIONS] bump [LEVELS..]
```

{{< hint info >}}
💡 參數 **[LEVELS..]** 接受 **major**、**minor** 和/或 **patch**！
{{< /hint >}}

## 🔍 eask cat

查看文件名。

位置參數 `[PATTERNS..]` 是一個通配符模式數組。

```sh
eask [GLOBAL-OPTIONS] cat [PATTERNS..]
```

{{< hint info >}}
💡 此命令使用包 [e2ansi](https://github.com/Lindydancer/e2ansi) 來完成語法高亮。
{{< /hint >}}

## 🔍 eask concat

將所有 Emacs Lisp 文件連接成一個文件。

```sh
eask [GLOBAL-OPTIONS] concat [FILES..]
```

## 🔍 eask loc

列印 LOC 信息。

```sh
eask [GLOBAL-OPTIONS] loc [FILES..]
```

# 🚩 文件

用於建立文檔站點的命令。

## 🔍 eask docs el2org

使用 [el2org][] 建置文檔。

```sh
eask [GLOBAL-OPTIONS] docs el2org [NAMES..]
```

# 🚩 執行

指令允許執行在 Eask 核心之上。

基本上，這可以讓你做任何你想做的事！

## 🔍 eask load

按順序加載 Emacs Lisp 文件。

```sh
eask [GLOBAL-OPTIONS] load [FILES..]
```

## 🔍 eask exec

使用給定的參數執行系統命令。

```sh
eask [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]
```

## 🔍 eask emacs

在合適的環境下執行emacs。

```sh
eask [GLOBAL-OPTIONS] emacs [ARGUMENTS ...]
```

## 🔍 eask eval

將 `FORM` 評估為 lisp 形式。

```sh
eask [GLOBAL-OPTIONS] eval [FORM]
```

## 🔍 eask repl

啟動一個 Elisp REPL。

```sh
eask [GLOBAL-OPTIONS] repl [FILES..]
```

別名: `ielm`

## 🔍 eask run script

運行腳本。

```sh
eask [GLOBAL-OPTIONS] run script [NAMES..]
```

## 🔍 eask run command

運行指令。

別名: `cmd`

```sh
eask [GLOBAL-OPTIONS] run command [NAMES..]
```

## 🔍 eask docker

在 Docker 容器中啟動指定的 Emacs 版本

```sh
eask [GLOBAL-OPTIONS] docker <VERSION> [ARGUMENTS ...]
```

例如：

```sh
eask docker 26.1 info
```

這與直接跳入 Emacs 26.1（在 docker 中）並執行 `eask info` 相同。

# 🚩 管理

指令能幫助你管理套件依賴.

## 🔍 eask archives

列出所有包源。

```sh
eask [GLOBAL-OPTIONS] archives
```

## 🔍 eask search

從包源中搜索包。

```sh
eask [GLOBAL-OPTIONS] search [QUEIRES..]
```

## 🔍 eask upgrade

升級所有軟件包。

```sh
eask [GLOBAL-OPTIONS] upgrade
```

## 🔍 eask list

使用樹狀依賴圖列出所有已安裝的包。

```sh
eask [GLOBAL-OPTIONS] list [--depth]
```

## 🔍 eask outdated

列出所有過時的包。

```sh
eask [GLOBAL-OPTIONS] outdated [--depth]
```

## 🔍 eask refresh

刷新包源。

```sh
eask [GLOBAL-OPTIONS] refresh
```

# 🚩 生成

生成用於開發的文件。

## 🔍 eask generate autoloads

生成一個 autoloads 文件。

將包自動加載到項目根目錄中的 `project-autoloads.el`。

```sh
eask [GLOBAL-OPTIONS] generate autoloads
```

`project` 是在 `Eask` 文件中聲明的項目名稱。 有關詳細信息，請參閱
[多文件包 (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)。

## 🔍 eask generate pkg-file

生成一個 pkg 文件。

將包描述符文件寫入項目根目錄中的 `project-pkg.el`。

別名: `pkg`, `pkg-el`

```sh
eask [GLOBAL-OPTIONS] generate pkg-file
```

`project` 是在 `Eask` 文件中聲明的項目名稱。 有關詳細信息，請參閱
[多文件包 (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)。

## 🔍 eask generate recipe

生成 recipe 文件。

```sh
eask [GLOBAL-OPTIONS] generate recipe [DESTINATION]
```

如果未指定 [DESTINATION]，則默認導出到 `/recipes` 文件夾。

## 🔍 eask generate license

生成 LICENSE 文件。

```sh
eask [GLOBAL-OPTIONS] generate license <name>
```

name` 是許可證的類型，請參閱 https://api.github.com/licenses 了解所有選擇。

{{< hint info >}}
💡 此命令使用包 [license-templates](https://github.com/jcs-elpa/license-templates) 生成忽略文件。
{{< /hint >}}

## 🔍 eask generate ignore

生成 `ignore` 文件。

```sh
eask [GLOBAL-OPTIONS] generate ignore <name>
```

{{< hint info >}}
💡 此命令使用包 [gitignore-templates](https://github.com/xuchunyang/gitignore-templates.el) 生成忽略文件。
{{< /hint >}}

## 🔍 eask generate test ert

為 [ert][]測試建立一個新的測試項目。

```sh
eask [GLOBAL-OPTIONS] generate test ert [NAMES..]
```

## 🔍 eask generate test ert-runner

為 [ert-runner][] 建立一個新的測試項目。

```sh
eask [GLOBAL-OPTIONS] generate test ert-runner [NAMES..]
```

## 🔍 eask generate test buttercup

為專案建立一個新的 [Buttercup][] 設定。

```sh
eask [GLOBAL-OPTIONS] generate test buttercup
```

## 🔍 eask generate test ecukes

為專案創建一個新的 [Ecukes][] 設定。

```sh
eask [GLOBAL-OPTIONS] generate test ecukes
```

## 🔍 eask generate workflow circle-ci

生成 [CircleCI][] 工作流 yaml 文件。

默認文件名為 `config.yml`。

```sh
eask [GLOBAL-OPTIONS] generate workflow circle-ci [--file]
```

這將在 `.circleci/` 下生成 yaml 文件！

## 🔍 eask generate workflow github

生成 [GitHub Actions][] 工作流 yaml 文件。

默認文件名為 `test.yml`。

```sh
eask [GLOBAL-OPTIONS] generate workflow github [--file]
```

這將在 `.github/workflow/` 下生成 yaml 文件！

## 🔍 eask generate workflow gitlab

生成 [GitLab Runner][] 工作流程 yaml 文件。

默認文件名為 `.gitlab-ci.yml`。

```sh
eask [GLOBAL-OPTIONS] generate workflow gitlab [--file]
```

## 🔍 eask generate workflow travis-ci

生成 [Travis CI][] 工作流 yaml 文件。

默認文件名為 `.travis.yml`。

```sh
eask [GLOBAL-OPTIONS] generate workflow travis-ci [--file]
```

# 🚩 連結

此包與本地文件系統的依賴關係之間的鏈接。 鏈接的依賴項避免了從遠程存檔下載依賴項的需要。
鏈接到的包必須有一個 `Eask` 文件或一個 `-pkg.el` 文件。

## 🔍 eask link add

將給定的 *source* 目錄鏈接到此項目的包目錄，在給定的 *package* 名稱下。

```sh
eask [GLOBAL-OPTIONS] link add <NAME> <PATH>
```

## 🔍 eask link delete

刪除本機連結的套件。

別名: `remove`

```sh
eask [GLOBAL-OPTIONS] link delete [NAMES..]
```

## 🔍 eask link list

列出所有鏈接。

```sh
eask [GLOBAL-OPTIONS] link list
```

# 🚩 清理

刪除建置過程中產生的各種檔案。

## 🔍 eask clean workspace

從當前 workspace 中刪除 `.eask` 。

別名: `.eask`

```sh
eask [GLOBAL-OPTIONS] clean workspace
```

⛔️ 不要指定選項 `--config, -c`，否則它會刪除你的整個 `~/.emacs.d`。

```elisp
eask clean workspace -g
```

## 🔍 eask clean elc

刪除所有 `.elc` 文件。 這將尊重您的 `Eask` 文件。

```sh
eask [GLOBAL-OPTIONS] clean elc
```

## 🔍 eask clean dist

刪除儲存建立輸出的 `dist` 目錄。

別名: `distribution`

```sh
eask [GLOBAL-OPTIONS] clean dist
```

## 🔍 eask clean autoloads

刪除生成的 autoload 文件。

```sh
eask [GLOBAL-OPTIONS] clean autoloads
```

## 🔍 eask clean pkg-file

刪除生成的 pkg 文件。

```sh
eask [GLOBAL-OPTIONS] clean pkg-file
```

## 🔍 eask clean log-file

刪除所有生成的日誌文件。

```sh
eask [GLOBAL-OPTIONS] clean log-file
```

## 🔍 eask clean all

此命令是所有其他清理命令的組合。

- `clean workspace`
- `clean elc`
- `clean dist`
- `clean autoloads`
- `clean pkg-file`
- `clean log-file`

別名: `everything`

```sh
eask [GLOBAL-OPTIONS] clean all
```

# 🚩 检查

對 Emacs 包進行 lint 的命令。

## 🔍 eask lint package

運行 [package-lint](https://github.com/purcell/package-lint).

```sh
eask [GLOBAL-OPTIONS] lint package [FILES..]
```

## 🔍 eask lint checkdoc

運行 checkdoc (自帶)。

```sh
eask [GLOBAL-OPTIONS] lint checkdoc [FILES..]
```

## 🔍 eask lint elint

運行 elint (自帶)。

```sh
eask [GLOBAL-OPTIONS] lint elint [FILES..]
```

## 🔍 eask lint elisp-lint

運行 [elisp-lint](https://github.com/gonewest818/elisp-lint).

```sh
eask [GLOBAL-OPTIONS] lint elisp-lint [FILES..]
```

這確實尊重 .dir-locals.el 文件！ 🎉

## 🔍 eask lint elsa

運行 [elsa](https://github.com/emacs-elsa/Elsa).

```sh
eask [GLOBAL-OPTIONS] lint lint elsa [FILES..]
```

## 🔍 eask lint indent

運行 indent-lint.

```sh
eask [GLOBAL-OPTIONS] lint indent [FILES..]
```

## 🔍 eask lint declare

運行 check-declare (自帶)。

```sh
eask [GLOBAL-OPTIONS] lint declare [FILES..]
```

## 🔍 eask lint regexps

Run [relint](https://github.com/mattiase/relint).

別名: `lint relint`

```sh
eask [GLOBAL-OPTIONS] lint regexps [FILES..]
```

## 🔍 eask lint keywords

運行 keywords checker (自帶).

```sh
eask [GLOBAL-OPTIONS] lint keywords
```

## 🔍 eask lint license

運行 license 檢查器。

```sh
eask [GLOBAL-OPTIONS] lint license
```

## 🔍 eask lint org

在 Org 檔案上運行 `org-lint`。

```sh
eask [GLOBAL-OPTIONS] lint org [FILES..]
```

# 🚩 測試框架

運行回歸/單元測試。

## 🔍 eask test activate

激活包； 用於測試包激活

```sh
eask [GLOBAL-OPTIONS] activate [FILES..]
```

{{< hint info >}}
💡 您可以傳入 **[FILES..]** 以便您可以全面測試您的包激活！

**[FILES..]** 將在包激活後加載。
{{< /hint >}}

## 🔍 eask test ert

運行 [ert][] 測試。

```sh
eask [GLOBAL-OPTIONS] test ert [FILES..]
```

## 🔍 eask test ert-runner

使用 [ert-runner][] 運行 [ert][] 測試。

```sh
eask [GLOBAL-OPTIONS] test ert-runner [FILES..] [-p PATTERN] [-t TAGS] [--reporter NAME]
```

{{< hint info >}}
💡 你可以傳入 **-p, --pattern**，僅運行符合該模式的測試！

```sh
eask test ert-runner -p some-specific-test
```

💡 你可以傳入 **-t, --tags**，僅運行帶有這些標籤的測試！

💡 你可以傳入 **--reporter** 來更換報告器；例如 `--reporter ert`。
{{< /hint >}}

## 🔍 eask test buttercup

運行 [buttercup][] 測試。

```sh
eask [GLOBAL-OPTIONS] test buttercup
```

## 🔍 eask test ecukes

運行 [ecukes][] 測試。

```sh
eask [GLOBAL-OPTIONS] test ecukes [FILES..]
```

## 🔍 eask test melpazoid

運行 [melpazoid][] 測試。

```sh
eask [GLOBAL-OPTIONS] test melpazoid [DIRECTORIES..]
```

{{< hint info >}}
💡 如果未傳入 **[DIRECTORIES..]**，它將使用目前工作空間。
{{< /hint >}}

# 🚩 格式化

格式化 Emacs 源文件的命令。

## 🔍 eask format elisp-autofmt

運行 [elisp-autofmt][] 格式器.

```sh
eask [GLOBAL-OPTIONS] format elisp-autofmt [FILES..]
```

## 🔍 eask format elfmt

運行 [elfmt][] 格式器.

```sh
eask [GLOBAL-OPTIONS] format elfmt [FILES..]
```

# 🚩 控制 DSL

控制 DSL 的指令列表。

## 🔍 eask source add

新增一個包源。

```sh
eask [GLOBAL-OPTIONS] source add <NAME> [URL]
```

## 🔍 eask source delete

移除一個包源。

別名: `remove`

```sh
eask [GLOBAL-OPTIONS] source delete <NAME>
```

## 🔍 eask source list

列出所有包源。

```sh
eask [GLOBAL-OPTIONS] source list
```

{{< hint info >}}
💡 指令與 `eask archives` 相同!
{{< /hint >}}

# 🚩 實用工具

其他輔助命令。

## 🔍 eask upgrade-eask

將 Eask 升級到最新版本。

別名: `upgrade-self`

```sh
eask [GLOBAL-OPTIONS] upgrade-eask
```

{{< hint warning >}}
💡 這只有在您從源代碼安裝時才有效！
{{< /hint >}}

## 🔍 eask locate

顯示 Eask 的安裝位置。

```sh
eask [GLOBAL-OPTIONS] locate
```

## 🔍 eask root

顯示包的安裝目錄。

```sh
eask [GLOBAL-OPTIONS] root
```

# 🚩 檢查器

檢查您的 Eask 文件的命令。

## 🔍 eask analyze

檢查 `Eask` 文件。

```sh
eask [GLOBAL-OPTIONS] analyze [FILES..]
```

例子:

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

有關更多詳細信息，請運行 `eask analyze --help`。

# 🚩 全域選項

以下選項適用於所有 Eask 命令：

## 🔍 --global, -g

這將使用 ~/.eask/ 而不是包開發環境。

這用於其他任務。 例如，`cat` 等。

```sh
eask -g [COMMAND]
```

## 🔍 --config, -c

這將使用 `~/.emacs.d/` 而不是包開發環境。

這用於為您的**Emacs 配置**做一些事情。 例如，包管理等。

```sh
eask -c [COMMAND]
```

## 🔍 --all, -a

啟用 `all` 標誌。

```sh
eask -a [COMMAND]
```

## 🔍 --quick, -q

乾淨地啟動而不加載配置文件。

```sh
eask -q [COMMAND]
```

## 🔍 --force, -f

強制執行命令。

強制卸載包 `dash` ，即使它是另一個包的依賴項

```sh
eask -f [COMMAND]
```

## 🔍 --debug

啟用調試信息。

這相當於：

```elisp
(setq debug-on-error t)
```

## 🔍 --strict

觸發錯誤代替警告。

例如，在 **eask compile** 中：

```elisp
(setq byte-compile-error-on-warn t)
```

## 🔍 --allow-error

在不終止 Emacs 的情況下繼續執行。

## 🔍 --insecure

使用 HTTP 而不是 HTTPS 連接存檔。

## 🔍 --timestamps

啟用/禁用時間戳。

## 🔍 --log-level

啟用/禁用日誌標頭。

## 🔍 --log-file, --lf

是否生成日誌文件。

## 🔍 --no-color

禁用顏色輸出。

## 🔍 --elapsed-time, --et

顯示每個操作之間經過的時間。

## 🔍 --verbose, -v `<integer>`

將詳細程度從 0 設置為 5。

```sh
eask --verbose 4 [COMMAND]
```

## 🔍 --version

顯示版本號。

## 🔍 --help

顯示幫助。

# 🚩 代理選項

## 🔍 --proxy `<proxy>`

為 HTTP 和 HTTPS 設置 Emacs 代理：

```sh
eask --proxy "localhost:8888" [COMMAND]
```

## 🔍 --http-proxy `<proxy>`

僅為 HTTP 設置 Emacs 代理。

## 🔍 --https-proxy `<proxy>`

僅為 HTTPS 設置 Emacs 代理。

## 🔍 --no-proxy `<pattern>`

不要對任何 URL 匹配模式使用代理。

`<pattern>` 是 Emacs 正則表達式。


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
