+++
title = "在 deepin 25 中使用 clangd 的坑：找不到系统头文件"
date = "2026-03-24"
description = "deepin 25 + clangd-19 配置踩坑记录"
tags = [
    "deepin","clangd","clang","cmake","vscode"
]
noToc = true
+++

## 经过

我在 deepin 25 上日常用 GCC 编译，VSCode 里 clangd 一直报系统头文件 `file not found`，但一直没细查。直到有次切到 `clang++-19` 编译项目，CMake 直接报错：

```txt
/usr/bin/ld: 找不到 -lstdc++: 没有那个文件或目录
```

这才意识到两个问题是同一个根因：标准库开发包没装全。

## 解决

安装对应版本的开发包即可：

```bash
sudo apt install libstdc++-13-dev
```

这里比较坑的是 deepin 预装了 `libstdc++-12-dev`，但 clangd 仍然用不了。  
我一开始先装了 `libc++-dev` 和 `libc++abi-dev`，但是编译仍然报错，看起来 clang 还是默认使用 libstdc++ 而非 libc++。

装完 `libstdc++-13-dev` 后 clangd 不再报系统头文件缺失，`clang++-19` 也能正常编译链接。

deepin 上的 gcc 版本是奇怪的 `4:13.2.0+really12.3.0-0deepin2`，不知道是不是系统库 gcc 降级导致的坑。
