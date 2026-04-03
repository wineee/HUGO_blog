+++
title = "几个非 NixOS 场景下的 Nix 小技巧：home-manager、system-manager 和 GPU"
date = "2026-04-03"
description = "从我的 nix-dotfiles 里抽出来的几个实用配置：nix run 启动 home-manager、跨架构复用 flake、system-manager 安装，以及 nix-system-graphics。"
tags = [
    "nix",
    "home-manager",
    "system-manager",
    "linux"
]
noToc = true
+++

最近整理了一下自己的 [`~/nix-dotfiles`](https://github.com/wineee/nix-dotfiles)，有几处在非 NixOS 机器上很好用，尤其适合只装了 Nix、但又想把用户环境和一部分系统环境一起声明式管理的人。

这篇主要记三个点：

1. `home-manager` 可以通过 flake 里的默认 app 包起来，直接 `nix run`
2. `system-manager` 可以直接由 `home-manager` 安装，再手动 `sudo` 应用
3. 图形程序这块更推荐 [`nix-system-graphics`](https://github.com/soupglasses/nix-system-graphics)，体验比 `nixGL` 顺很多

<!--more-->

## 1. `home-manager` 的几个小技巧

### 1.1 用 `apps.${system}.default` 包一层，仓库里直接 `nix run`

我现在的 flake 会导出一个默认 app，这样在仓库根目录只要执行：

```bash
nix run
```

本质上还是调用 `home-manager switch --flake . --impure`，只是把这层入口藏进 flake 了：

```nix
apps.${system}.default = {
  type = "app";
  program = "${pkgs.writeShellScript "hm-switch" ''
    exec ${home-manager.packages.${system}.home-manager}/bin/home-manager switch --flake . --impure "$@"
  ''}";
};
```

这样做有两个好处：

- 新机器初始化时，不用先手敲一遍 `nix shell nixpkgs#home-manager`
- 平时更新配置时，进入仓库直接 `nix run`

另外，`home.nix` 里我开了：

```nix
programs.home-manager.enable = true;
```

这意味着后续 `home-manager` 本体也是通过 `home-manager` 自己管理的。第一次靠 flake 拉起来，后面就比较顺手了。

### 1.2 用 `flake-utils.lib.eachDefaultSystemPassThrough` 复用 x86/arm Linux 配置

如果你同时在 `x86_64-linux` 和 `aarch64-linux` 机器上用同一套配置，这个写法很省事：

```nix
flake-utils.lib.eachDefaultSystemPassThrough (system:
  let
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
  in
  {
    homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [ ./home.nix ];
      extraSpecialArgs = { inherit inputs username system; };
    };

    apps.${system}.default = { ... };
  })
```

这里的重点不是“支持很多系统”，而是“同一份 `home.nix` 不用拆两套”。  
像 `pkgs.stdenv.hostPlatform.system` 这种值会跟着当前系统走，包也会自动按架构解析。

### 1.3 不同机器用户名不同，但还想共用一份配置

一个常见问题是：你想把 dotfiles 放进公开仓库，但家里、公司、服务器上的用户名又不完全一样。

我这里偷了个懒，直接从环境变量里拿用户名：

```nix
let
  username =
    let
      envUser = builtins.getEnv "USER";
    in
    if envUser == "" then "rewine" else envUser;
in
```

然后在 `home.nix` 里统一写：

```nix
home.username = username;
home.homeDirectory = "/home/${username}";
```

因为 `builtins.getEnv` 属于 impurity，所以调用时需要带上：

```bash
home-manager switch --flake . --impure
```

或者像上面那样，直接在 `nix run` 的默认 app 里把 `--impure` 固化进去。

这招很朴素，但很好用。对于“只是用户名不同，其它都想共用”的场景，足够了。

## 2. `system-manager` 可以直接由 `home-manager` 安装

[`system-manager`](https://github.com/numtide/system-manager) 可以理解成“非 NixOS 上更像 NixOS module 的系统层配置工具”。它的 README 里也是这么定位的：如果你熟悉 `home-manager`，那可以把它理解成“面向整个系统的版本”。

我的做法不是每次都用上游的：

```bash
nix run 'github:numtide/system-manager' -- switch --flake .
```

而是先把它放进 `home.packages`：

```nix
home.packages = with pkgs; [
  (inputs.system-manager.packages.${pkgs.stdenv.hostPlatform.system}.default)
];
```

这样应用用户配置后，`system-manager` 二进制就会出现在：

```txt
/home/username/.nix-profile/bin/system-manager
```

然后再手动提权执行：

```bash
sudo /home/username/.nix-profile/bin/system-manager switch --flake .
```

我自己的 flake 里对应输出大概是这样：

```nix
systemConfigs.default = system-manager.lib.makeSystemConfig {
  modules = [
    nix-system-graphics.systemModules.default
    {
      nix.settings.experimental-features = "nix-command flakes";
    }
    ./system.nix
  ];
  extraSpecialArgs = { inherit inputs system; };
};
```

而 `system.nix` 里可以直接写：

```nix
{ pkgs, system, lib, ... }:
{
  config = {
    nixpkgs.hostPlatform = system;
    system-manager.allowAnyDistro = true;

    environment.systemPackages = with pkgs; [
      neovim
      fastfetch
      ripgrep
      fd
    ];
  };
}
```

这样分层还挺清楚：

- 用户态东西交给 `home-manager`
- `/etc`、systemd、系统级包之类交给 `system-manager`

## 3. 图形程序更推荐 `nix-system-graphics`

项目地址：<https://github.com/soupglasses/nix-system-graphics>

这个项目我很推荐，尤其是你在普通 Linux 发行版上用 Nix 装 GUI 软件、窗口管理器、终端模拟器的时候。

先说结论：它比 `nixGL` 更省心。

`nixGL` 的典型使用方式是这样：

```bash
nixGL qtcreator
```

问题也很明显：

- 你得记住“这个程序要不要套一层 `nixGL`”
- 桌面启动器、文件关联、别的 GUI 程序拉起它时，不一定好包这层命令
- 一个 Nix 程序再去启动另一个图形程序时，体验很别扭

`nix-system-graphics` 的思路不一样。它通过 `system-manager` 填充 `/run/opengl-driver`，跟 NixOS 的处理方式一致。上游 README 里明确提到，这样就“不需要再对 Nix store 里的程序做 patch 或 wrapper”，Nix 构建的程序会直接使用 `/run/opengl-driver` 去找 `libGL` 和 `libvulkan`。

直接看配置，最小可用大概就是：

```nix
systemConfigs.default = system-manager.lib.makeSystemConfig {
  modules = [
    nix-system-graphics.systemModules.default
    {
      config = {
        nixpkgs.hostPlatform = system;
        system-manager.allowAnyDistro = true;
        system-graphics.enable = true;
      };
    }
    ./system.nix
  ];
};
```

然后应用：

```bash
sudo /home/username/.nix-profile/bin/system-manager switch --flake .
```

我自己现在也是这么配的。优点很直接：

- 不需要把每个图形程序都改成 `nixGL xxx`
- `nix run nixpkgs#xxx` 这类临时启动方式更自然
- 对启动器启动、Nix 程序拉起系统程序这类场景更友好

如果你之前已经被 `nixGL` 的 wrapper 绕烦了，这个项目值得直接试一下。

## 小结

这套组合我觉得很适合“非 NixOS，但想尽量往声明式配置靠”的日常桌面环境：

- `home-manager` 负责用户环境，并通过 `nix run` 降低入口成本
- `system-manager` 负责系统层配置
- `nix-system-graphics` 补上 GUI/GPU 体验里最烦的一块

相关项目：

- [`home-manager`](https://github.com/nix-community/home-manager)
- [`system-manager`](https://github.com/numtide/system-manager)
- [`nix-system-graphics`](https://github.com/soupglasses/nix-system-graphics)
- 我的配置：[`wineee/nix-dotfiles`](https://github.com/wineee/nix-dotfiles)
