---
title: 💾 安裝 Eask
weight: 200
---

本文檔將指導您完成 Eask 的安裝。

{{< toc >}}

安裝 Eask在 macOS、Linux、Windows、BSD 以及任何可以執行 [Node.js][]。

## 💾 預建置檔案

從 [Eask Releases](https://github.com/emacs-eask/cli/releases) 下載適合您平台的版本。
下載後，二進製文件可以從任何地方運行。 您無需將其安裝在全球位置。 這適用於您沒有特權帳戶的共享主機和其他系統。

理想情況下，您應該將它安裝在 `PATH` 中的某個位置以便於使用。 `/usr/local/bin` 是最有可能的位置。

## 💾 使用終端

在 macOS 或 Linux:

```sh
$ curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
```

在 Windows:

```sh
$ curl.exe -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.bat | cmd /Q
```

## 💾 包管理器

### 📦 npm (跨平台)

如果您的機器上安裝了 [npm](https://www.npmjs.com/)，您可以使用以下一行代碼安裝 Eask：

```sh
$ npm install -g @emacs-eask/cli
```

### 📦 Homebrew (macOS 或 Linux)

[Homebrew](https://brew.sh/) 是一個適用於 macOS 和 Linux 的免費開源套件管理器。
這將安裝 Eask CLI 的擴充版本：

```sh
$ brew tap emacs-eask/cli https://github.com/emacs-eask/packaging
$ brew install eask-cli
```

### 📦 MacPorts (macOS)

[MacPorts](https://www.macports.org/) 是一款適用於 macOS 的免費開源套件管理器。
這將安裝 Eask 的擴充版本：

```sh
$ sudo port install eask-cli
```

### 📦 Chocolatey (Windows)

如果您的計算機上安裝了 [Chocolatey](https://chocolatey.org/)，則可以使用以下一行代碼安裝 Eask：

```sh
$ choco install eask-cli
```

### 📦 Scoop (Windows)

[Scoop](https://scoop.sh/) 是一個適用於 Windows 的免費開源套件管理器。
這將安裝 Eask 的擴充版本：

```sh
$ scoop bucket add emacs-eask/cli https://github.com/emacs-eask/packaging
$ scoop install eask-cli
```

### 📦 Winget (Windows)

WIP

## 💾 從原始碼構建

### 🚩 前置工具

* [Git][]
* [Node.js][]
* [npm][]

或者，您可以直接從這個代碼庫克隆它:

```sh
# 克隆這個代碼庫
$ git clone https://github.com/emacs-eask/cli eask-cli

# 將工作目錄更改為 eask-cli
$ cd eask-cli

# 安裝所有依賴
$ npm install
```

### 🏡 設置

確保根據您的系統設置環境路徑變量:

在 Linux/macOS 上，

```sh
export PATH="path/to/eask/bin:$PATH"
```

在 Windows 上，

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

正確設置後，嘗試 `eask --version` 然後您應該會看到當前 eask 的版本號！ 🎉🎊

[Git]: https://git-scm.com/
[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/
