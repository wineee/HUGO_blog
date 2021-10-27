---
title: 在 nixos 中使用 telega 进行聊天
date: "2021-10-27"
math: false
tags:  ["nixos","emacs"]
---

telega 是一个 emacs 上的 Telegram 客户端，在 nixos 中，telega 包过于老旧，在 unstable 中已经无了，下面尝试使用传统方法安装 telega。

### 下载源代码

```bash
cd ~/.emacs.d/site-lisp
git clone git@github.com:zevlg/telega.el.git
```

参考[文档](https://zevlg.github.io/telega.el/)进行配置

<!--more-->

一个例子是（我以前抄的），注意改对 load-path：

```lisp
(use-package telega
  :load-path "~/.emacs.d/site-lisp/telega.el"
  :commands (telega)
  :init (setq telega-proxies
	      '((:server "localhost"
			 :port "1089"
                         :enable t
                         :type (:@type "proxyTypeSocks5")))
              telega-chat-show-avatars nil)
  (setq telega-chat-fill-column 65)
  (setq telega-emoji-use-images t)
  ;;(setq telega-server-libs-prefix "/nix/store/8n54mnnizmzvi7b3bhv31fc3hr0fcl9i-tdlib-1.7.0/lib")
  :config
  (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend)
  (with-eval-after-load 'company (add-hook 'telega-chat-mode-hook (lambda ()
                                                                    (make-local-variable 'company-backends)
                                                                    (dolist (it '(telega-company-botcmd telega-company-emoji))
                                                                      (push it company-backends)))))
  (with-eval-after-load 'all-the-icons (add-to-list 'all-the-icons-mode-icon-alist '(telega-root-mode all-the-icons-fileicon "telegram"
                                                                                                      :heigt 1.0
                                                                                                      :v-adjust -0.2
                                                                                                      :face all-the-icons-yellow))
                        (add-to-list 'all-the-icons-mode-icon-alist '(telega-chat-mode all-the-icons-fileicon "telegram"
                                                                                       :heigt 1.0
                                                                                       :v-adjust -0.2
                                                                                       :face all-the-icons-blue)))
  (telega-notifications-mode t)
  (telega-mode-line-mode 1)
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (toggle-truncate-lines +1)
              (display-line-numbers-mode -1)))
  (add-hook 'telega-root-mode-hook
            (lambda ()
              (toggle-truncate-lines +1)
              (display-line-numbers-mode -1)
              (toggle-truncate-lines -1)))
  (define-key telega-msg-button-map "k" nil))

```

理论上安装 tdlib 后，`M-x` telega 就能编译 **telega-server** 并运行。在 nixos 需要手动编译。

### 手动编译 **telega-server**

```bash
nix-shell -p cmake gperf pkg-config tdlib
cd ~/.emacs.d/site-lisp/telega.el/server
make install
```

注意 make install 会放进 **$(HOME)**/.telega 中

### 解决版本不一致问题

重新 `M-x` telega，提示 tdlib 需要 ≥= 1.7.7, 而 nixpkgs 中的版本是 1.7.0

有两种方法，降级 telega 或者 升级 tdlib。

1. telega 使用 git 管理，降级很容易，git log 找到对应版本的 commit log，之后 git checkout。但缺点很明显，新功能和bug修复没了，而且可能和服务器交互出问题。

2. 最好还是升级一下 tdlib， 我去翻了翻 nixpkgs 中的构建脚本（[pkgs/development/libraries/tdlib/default.nix](https://github.com/NixOS/nixpkgs/pull/143176/files#diff-a956d0a996ce4ca6c593bc126b5ab94f1d5feb8751d7ae4506452dc2524a4c8a)），它是使用 [fetchFromGitHub](https://nixos.org/manual/nixpkgs/stable/#fetchfromgithub) 构建的。

查看文档 `fetchFromGitHub` 有这么几个参数：

- `owner`is a string corresponding to the GitHub user or organization that controls this 
- `repo`corresponds to the name of the software repository. 
- `rev`corresponds to the Git commit hash or tag (e.g`v1.0`) that will be downloaded from Git. 
- `sha256`corresponds to the hash of the extracted directory. 

需要修改的是 rev 和 sha256。

rev： tdlib 的 tag 更新太慢，改用 commit 的 hash，在 github 点进一个 commit 的记录里就能找到它的 hash

sha 的计算，参考：[https://github.com/NixOS/nix/issues/1880](https://github.com/NixOS/nix/issues/1880)

```bash
nix-prefetch-url --unpack https://github.com/tdlib/td/archive/a68d8e77efb03896f3a04316c47136b0bab7df.tar.gz
```

在 nixpkgs 目录下执行：

```bash
nix-build -A tdlib
```
同目录的 result 保存构建结果。

如果需要安装可以执行

```bash
nix-env -f . -iA tdlib
```

