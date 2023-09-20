---
title: 🚩 命令和選項
weight: 300
---

{{< toc >}}

**eask** 程序的一般語法是：

```sh
$ eask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

# 🚩 創建

## 🔍 eask create package

使用默認的“Eask”文件和 CI/CD 支持創建一個新的 elisp 項目。

```sh
$ eask [GLOBAL-OPTIONS] create package <name>
```

{{< hint info >}}
💡 模板項目位於 https://github.com/emacs-eask/template-elisp。
{{< /hint >}}

## 🔍 eask create elpa

使用 [github-elpa](https://github.com/10sr/github-elpa) 創建一個新的 ELPA。

```sh
$ eask [GLOBAL-OPTIONS] create elpa <name>
```

{{< hint info >}}
💡 模板項目位於 https://github.com/emacs-eask/template-elpa。
{{< /hint >}}

# 🚩 核心

經常使用未分類的命令。

## 🔍 eask init

初始化當前目錄以開始使用 Eask。

```sh
$ eask [GLOBAL-OPTIONS] init
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

如果您已有 [Cask][] 項目，您可以將 Cask 文件轉換為 Eask 文件：

```
$ eask init --from cask /path/to/Cask
```

如果您已有 [Keg][] 項目，您可以將 Keg 文件轉換為 Eask 文件：

```
$ eask init --from keg /path/to/Keg
```

**[推薦]** 如果您已有 elisp 項目，您可以將 `.el` 文件轉換為 Eask 文件：

```
$ eask init --from source /path/to/source.el
```

{{< hint ok >}}
💡 有關更多 Eask 文件示例，請參閱[示例](https://emacs-esk.github.io/examples)部分！
{{< /hint >}}

## 🔍 eask info

顯示有關項目或配置的信息。

```sh
$ eask [GLOBAL-OPTIONS] info
```

## 🔍 eask status

顯示工作區的狀態。

```sh
$ eask [GLOBAL-OPTIONS] status
```

## 🔍 eask install-deps

安裝所有依賴項。

別名: `install-dependencies`, `prepare`

```sh
$ eask [GLOBAL-OPTIONS] install-deps [--dev]
```

{{< hint ok >}}
💡 指定選項 [--dev] 從開發範圍安裝依賴項。
{{< /hint >}}

## 🔍 eask install

安裝軟件包。

```sh
$ eask [GLOBAL-OPTIONS] install [PACKAGES..]
```

通過指定參數安裝包：

```sh
$ eask install auto-complete helm magit
```

否則，它將安裝當前開發的包：

```sh
$ eask install
```

## 🔍 eask uninstall

卸載/刪除包。

```sh
$ eask [GLOBAL-OPTIONS] uninstall [PACKAGES..]
```

通過指定參數卸載軟件包：

```sh
$ eask uninstall dash f s
```

否則，它將從當前開發中卸載包：

```sh
$ eask uninstall
```

## 🔍 eask package

構建包工件。

```sh
$ eask package [DESTINATION]
```

如果未指定 [DESTINATION]，則默認導出到 `/dist` 文件夾。

## 🔍 eask compile

字節編譯文件。

```sh
$ eask compile [FILES..]
```

通過指定參數編譯文件：

```sh
$ eask compile file-1.el file-2.el
```

或者編譯已經在你的 `Eask` 文件中指定的文件。

```sh
$ eask compile
```

## 🔍 eask package-directory

打印包目錄的路徑，其中安裝了所有依賴項。

```sh
$ eask [GLOBAL-OPTIONS] package-directory
```

## 🔍 eask path

打印此項目的 `PATH` 環境變量。

別名: `exec-path`

```sh
$ eask [GLOBAL-OPTIONS] path [PATTERNS..]
```

或者，您可以傳入 `[PATTERNS..]` 來執行搜索。

## 🔍 eask load-path

打印包含當前項目依賴項的加載路徑。

```sh
$ eask [GLOBAL-OPTIONS] load-path [PATTERNS..]
```

或者，您可以傳入 `[PATTERNS..]` 來執行搜索。

## 🔍 eask files

打印所有包文件的列表。

```sh
$ eask [GLOBAL-OPTIONS] files [PATTERNS..]
```

如果定義了 `[PATTERNS..]` ，它將顯示與該模式匹配的文件。

## 🔍 eask load

按順序加載 Emacs Lisp 文件。

```sh
$ eask [GLOBAL-OPTIONS] load [FILES..]
```

## 🔍 eask recipe

建議 recipe 格式。

```sh
$ eask [GLOBAL-OPTIONS] recipe [FILES..]
```

## 🔍 eask keywords

列出可在標題部分中使用的可用關鍵字。

```sh
$ eask [GLOBAL-OPTIONS] keywords
```

## 🔍 eask cat

查看文件名。

位置參數 `[PATTERNS..]` 是一個通配符模式數組。

```sh
$ eask [GLOBAL-OPTIONS] cat [PATTERNS..]
```

{{< hint info >}}
💡 此命令使用包 [e2ansi](https://github.com/Lindydancer/e2ansi) 來完成語法高亮。
{{< /hint >}}

## 🔍 eask concate

將所有 Emacs Lisp 文件連接成一個文件。

```sh
$ eask [GLOBAL-OPTIONS] concate [FILES..]
```

## 🔍 eask run

運行腳本。

```sh
$ eask [GLOBAL-OPTIONS] run [FILES..]
```

別名: `run-script`

# 🚩 執行

## 🔍 eask exec

使用給定的參數執行系統命令。

```sh
$ eask [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]
```

## 🔍 eask emacs

在合適的環境下執行emacs。

```sh
$ eask [GLOBAL-OPTIONS] emacs [ARGUMENTS ...]
```

## 🔍 eask eval

將 `FORM` 評估為 lisp 形式。

```sh
$ eask [GLOBAL-OPTIONS] eval [FORM]
```

## 🔍 eask docker

在 Docker 容器中啟動指定的 Emacs 版本

```sh
$ eask [GLOBAL-OPTIONS] docker <VERSION> [ARGUMENTS ...]
```

例如：

```sh
$ eask docker 26.1 info
```

這與直接跳入 Emacs 26.1（在 docker 中）並執行 `eask info` 相同。

# 🚩 管理

## 🔍 eask archives

列出所有包檔案。

```sh
$ eask [GLOBAL-OPTIONS] archives
```

## 🔍 eask search

從檔案中搜索包。

```sh
$ eask [GLOBAL-OPTIONS] search [QUEIRES..]
```

## 🔍 eask upgrade

升級所有軟件包。

```sh
$ eask [GLOBAL-OPTIONS] upgrade
```

## 🔍 eask list

列出包。

```sh
$ eask [GLOBAL-OPTIONS] list [--depth]
```

## 🔍 eask outdated

列出所有過時的包。

```sh
$ eask [GLOBAL-OPTIONS] outdated [--depth]
```

## 🔍 eask refresh

刷新包 archives。

```sh
$ eask [GLOBAL-OPTIONS] refresh
```

# 🚩 生成

生成用於開發的文件。

## 🔍 eask generate autoloads

生成 autoload 文件。

將包自動加載到項目根目錄中的 `project-autoloads.el`。

```sh
$ eask [GLOBAL-OPTIONS] generate autoloads
```

`project` 是在 `Eask` 文件中聲明的項目名稱。 有關詳細信息，請參閱
[多文件包 (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)。

## 🔍 eask generate pkg-file

生成 pkg 文件。

將包描述符文件寫入項目根目錄中的 `project-pkg.el`。

別名: `pkg`, `pkg-el`

```sh
$ eask [GLOBAL-OPTIONS] generate pkg-file
```

`project` 是在 `Eask` 文件中聲明的項目名稱。 有關詳細信息，請參閱
[多文件包 (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)。

## 🔍 eask generate license

生成 LICENSE 文件。

```sh
$ eask [GLOBAL-OPTIONS] generate license <name>
```

name` 是許可證的類型，請參閱 https://api.github.com/licenses 了解所有選擇。

{{< hint info >}}
💡 此命令使用包 [license-templates](https://github.com/jcs-elpa/license-templates) 生成忽略文件。
{{< /hint >}}

## 🔍 eask generate ignore

生成忽略文件。

```sh
$ eask [GLOBAL-OPTIONS] generate ignore <name>
```

{{< hint info >}}
💡 此命令使用包 [gitignore-templates](https://github.com/xuchunyang/gitignore-templates.el) 生成忽略文件。
{{< /hint >}}

## 🔍 eask generate workflow circle-ci

生成 CircleCI 工作流 yaml 文件。

默認文件名為 `config.yml`。

```sh
$ eask [GLOBAL-OPTIONS] generate workflow circle-ci [--file]
```

這將在 `.circleci/` 下生成 yaml 文件！

## 🔍 eask generate workflow github

生成 GitHub Actions 工作流 yaml 文件。

默認文件名為 `test.yml`。

```sh
$ eask [GLOBAL-OPTIONS] generate workflow github [--file]
```

這將在 `.github/workflow/` 下生成 yaml 文件！

## 🔍 eask generate workflow gitlab

生成 GitLab Runner 工作流程 yaml 文件。

默認文件名為 `.gitlab-ci.yml`。

```sh
$ eask [GLOBAL-OPTIONS] generate workflow gitlab [--file]
```

## 🔍 eask generate workflow travis-ci

生成 Travis CI 工作流 yaml 文件。

默認文件名為 `.travis.yml`。

```sh
$ eask [GLOBAL-OPTIONS] generate workflow travis-ci [--file]
```

# 🚩 連結

此包與本地文件系統的依賴關係之間的鏈接。 鏈接的依賴項避免了從遠程存檔下載依賴項的需要。
鏈接到的包必須有一個 `Eask` 文件或一個 `-pkg.el` 文件。

## 🔍 eask link add <name> <path>

將給定的 *source* 目錄鏈接到此項目的包目錄，在給定的 *package* 名稱下。

```sh
$ eask [GLOBAL-OPTIONS] link add <name> <path>
```

## 🔍 eask link delete [name..]

刪除給定包的鏈接。

```sh
$ eask [GLOBAL-OPTIONS] link delete [names..]
```

## 🔍 eask link list

列出所有鏈接。

```sh
$ eask [GLOBAL-OPTIONS] link list
```

# 🚩 清理

## 🔍 eask clean workspace

從當前工作區中刪除 `.eask` 。

```sh
$ eask [GLOBAL-OPTIONS] clean workspace
```

⛔️ 不要指定選項 `--config, -c`，否則它會刪除你的整個 `~/.emacs.d`。


```elisp
$ eask clean workspace -g
```

別名: `.eask`

## 🔍 eask clean elc

刪除所有 `.elc` 文件。 這將尊重您的 `Eask` 文件。

```sh
$ eask [GLOBAL-OPTIONS] clean elc
```

## 🔍 eask clean dist

刪除 dist 子目錄。

```sh
$ eask [GLOBAL-OPTIONS] clean dist
```

別名: `distribution`

## 🔍 eask clean autoloads

刪除生成的 autoload 文件。

```sh
$ eask [GLOBAL-OPTIONS] clean autoloads
```

## 🔍 eask clean pkg-file

刪除生成的 pkg 文件。

```sh
$ eask [GLOBAL-OPTIONS] clean pkg-file
```

## 🔍 eask clean log-file

刪除所有生成的日誌文件。

```sh
$ eask [GLOBAL-OPTIONS] clean log-file
```

## 🔍 eask clean all

此命令是所有其他清理命令的組合。

- `clean workspace`
- `clean elc`
- `clean dist`
- `clean autoloads`
- `clean pkg-file`
- `clean log-file`

```sh
$ eask [GLOBAL-OPTIONS] clean all
```

別名: `everything`

# 🚩 检查器

對 Emacs 包進行 lint 的命令。

## 🔍 eask lint package

運行 [package-lint](https://github.com/purcell/package-lint).

```sh
$ eask [GLOBAL-OPTIONS] lint package [FILES..]
```

## 🔍 eask lint checkdoc

運行 checkdoc (自帶).

```sh
$ eask [GLOBAL-OPTIONS] lint checkdoc [FILES..]
```

## 🔍 eask lint elint

運行 elint (自帶).

```sh
$ eask [GLOBAL-OPTIONS] lint elint [FILES..]
```

## 🔍 eask lint elisp-lint

運行 [elisp-lint](https://github.com/gonewest818/elisp-lint).

```sh
$ eask [GLOBAL-OPTIONS] lint elisp-lint [FILES..]
```

這確實尊重 .dir-locals.el 文件！ 🎉

## 🔍 eask lint elsa

運行 [elsa](https://github.com/emacs-elsa/Elsa).

```sh
$ eask [GLOBAL-OPTIONS] lint lint elsa [FILES..]
```

## 🔍 eask lint indent

運行 indent-lint.

```sh
$ eask [GLOBAL-OPTIONS] lint indent [FILES..]
```

## 🔍 eask lint keywords

運行 keywords checker (自帶).

```sh
$ eask [GLOBAL-OPTIONS] lint keywords
```

## 🔍 eask lint license

運行 license check.

```sh
$ eask [GLOBAL-OPTIONS] lint license
```

## 🔍 eask lint declare

運行 check-declare (自帶).

```sh
$ eask [GLOBAL-OPTIONS] lint declare [FILES..]
```

## 🔍 eask lint regexps

Run [relint](https://github.com/mattiase/relint).

```sh
$ eask [GLOBAL-OPTIONS] lint regexps [FILES..]
```

別名: `lint relint`

# 🚩 測試框架

## 🔍 eask test activate

激活包； 用於測試包激活

```sh
$ eask [GLOBAL-OPTIONS] activate [FILES..]
```

{{< hint info >}}
💡 您可以傳入 **[FILES..]** 以便您可以全面測試您的包激活！

**[FILES..]** 將在包激活後加載。
{{< /hint >}}

## 🔍 eask test ert

```sh
$ eask [GLOBAL-OPTIONS] test ert [FILES..]
```

## 🔍 eask test ert-runner

```sh
$ eask [GLOBAL-OPTIONS] test ert-runner
```

## 🔍 eask test buttercup

```sh
$ eask [GLOBAL-OPTIONS] test buttercup
```

# 🚩 實用工具

其他輔助命令。

## 🔍 eask upgrade-eask

將 Eask 升級到最新版本。

別名: `upgrade-self`

```sh
$ eask [GLOBAL-OPTIONS] upgrade-eask
```

{{< hint warning >}}
💡 這只有在您從源代碼安裝時才有效！
{{< /hint >}}

## 🔍 eask locate

顯示 Eask 安裝位置。

```sh
$ eask [GLOBAL-OPTIONS] locate
```

# 🚩 Checker

檢查您的 Eask 文件的命令。

## 🔍 eask check-eask

檢查 `Eask` 文件。

```sh
$ eask [GLOBAL-OPTIONS] check-eask [FILES..]
```

例子:

```bash
# lint all Eask-files in the current directory and subdirectories
eask check-eask
# lint specific files
eask check-eask Eask Eask.27
# lint all Eask-files in specified directory and subdirectories
eask check-eask src/
# print result as JSON
eask check-eask --json
```

有關更多詳細信息，請運行 `eask check-eask --help`。

# 🚩 全域選項

以下選項適用於所有 Eask 命令：

## 🔍 --global, -g

這將使用 ~/.eask/ 而不是包開發環境。

這用於其他任務。 例如，`cat` 等。

```sh
$ eask -g [COMMAND]
```

## 🔍 --config, -c

這將使用 `~/.emacs.d/` 而不是包開發環境。

這用於為您的**Emacs 配置**做一些事情。 例如，包管理等。

```sh
$ eask -c [COMMAND]
```

## 🔍 --all, -a

啟用 `all` 標誌。

```sh
$ eask -a [COMMAND]
```

## 🔍 --quick, -q

乾淨地啟動而不加載配置文件。

```sh
$ eask -q [COMMAND]
```

## 🔍 --force, -f

強制執行命令。

強制卸載包 `dash` ，即使它是另一個包的依賴項

```sh
$ eask -f [COMMAND]
```

## 🔍 --debug

啟用調試信息。

這相當於：

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
$ eask --verbose 4 [COMMAND]
```

## 🔍 --version

顯示版本號。

## 🔍 --help

顯示幫助。

# 🚩 代理選項

## 🔍 --proxy `<proxy>`

為 HTTP 和 HTTPS 設置 Emacs 代理：

```sh
$ eask --proxy "localhost:8888" [COMMAND]
```

## 🔍 --http-proxy `<proxy>`

僅為 HTTP 設置 Emacs 代理。

## 🔍 --https-proxy `<proxy>`

僅為 HTTPS 設置 Emacs 代理。

## 🔍 --no-proxy `<pattern>`

不要對任何 URL 匹配模式使用代理。

`<pattern>` 是 Emacs 正則表達式。


[Cask]: https://github.com/cask/cask
[Keg]: https://github.com/conao3/keg.el
