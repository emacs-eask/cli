---
title: 💾 安裝 Eask
weight: 200
---

本文檔將指導您完成 Eask 的安裝。

{{< toc >}}

## 💾 快速安裝

### 二進制（跨平台）

從 [Eask Releases](https://github.com/emacs-eask/cli/releases) 下載適合您平台的版本。
下載後，二進製文件可以從任何地方運行。 您無需將其安裝在全球位置。 這適用於您沒有特權帳戶的共享主機和其他系統。

理想情況下，您應該將它安裝在 `PATH` 中的某個位置以便於使用。 `/usr/local/bin` 是最有可能的位置。

### 使用 Shell （macOS 和 Linux）

```sh
$ curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
```

### 使用 Shell (Windows)

```sh
$ curl.exe -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.bat | cmd /Q
```

### npm

如果您的機器上安裝了 [npm](https://www.npmjs.com/)，您可以使用以下一行代碼安裝 Eask：

```sh
$ npm install -g @emacs-eask/cli
```

### Homebrew (macOS or Linux)

[Homebrew](https://brew.sh/) 是一個適用於 macOS 和 Linux 的免費開源套件管理器。
這將安裝 Eask CLI 的擴充版本：

```sh
$ brew tap https://github.com/emacs-eask/packaging
$ brew install eask-cli
```

### MacPorts (macOS)

WIP

### Chocolatey (Windows)

如果您的計算機上安裝了 [Chocolatey](https://chocolatey.org/)，則可以使用以下一行代碼安裝 Eask：

```sh
$ choco install eask-cli
```

### Scoop (Windows)

WIP

## 💾 Source

### 🚩 Prerequisite Tools

* [Git](https://git-scm.com/)
* [Node.js](https://nodejs.org/en/)
* [npm](https://www.npmjs.com/)

或者，您可以直接從這個代碼庫克隆它:

```sh
# 克隆這個代碼庫
$ git clone https://github.com/emacs-eask/cli eask

# 將工作目錄更改為eask
$ cd eask

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
