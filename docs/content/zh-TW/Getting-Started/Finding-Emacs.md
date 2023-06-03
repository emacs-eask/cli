---
title: 🔭 尋找 Emacs
weight: 150
---

默認情況下，會為默認的 Emacs 安裝軟件包，即“emacs”命令後面的軟件包。 要選擇不同的
Emacs，請將環境變量“EMACS”設置為要使用的 `Emacs` 的命令名稱或可執行路徑：

```sh
$ EMACS="emacs26.1" eask command
```

請注意，已安裝的依賴項受 Emacs 版本的影響。 因此，在版本之間切換時，您必須為每個版本安裝依賴項：

```sh
$ EMACS="emacs26.3" eask install
```

不幸的是，在某些情況下，Emacs 本身會以與 **eask** 衝突的方式重置 `EMACS` 變量，在這種情況下，
您可以改用環境變量 `EASK_EMACS`。 具體來說，此問題會影響：Emacs-26，對於 `M-x compile`、
`M-x shell` 或 `M-x term`，對於 Emacs-27 和 Emacs-28 僅適用於 `M-x term`。
