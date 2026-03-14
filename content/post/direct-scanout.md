---
title: "Direct Scan-out（直扫输出）"
date: 2026-03-14
tags: ["Wayland", "wlroots", "DRM", "KMS", "Linux"]
draft: true
---

Direct scan-out（直扫输出）是显示系统里非常重要的性能优化之一。核心思想是：**让显示控制器直接扫描应用的 framebuffer，从而绕过 compositor 的合成路径**。这样可以减少一次合成渲染和显存写入，降低延迟与带宽占用。

## 普通合成路径 vs Direct scan-out

常规合成路径大致是：应用渲染到自己的 buffer → compositor 把多个 surface 合成到一个 framebuffer → DRM/KMS 扫描输出到屏幕。这个过程通常会引入一次额外的合成渲染与写入。

Direct scan-out 会在满足条件时跳过合成：compositor 直接把应用的 buffer 绑定为 scanout buffer，让显示引擎去读这块内存。

## 硬件基础：KMS Plane

Direct scan-out 能成立，依赖 DRM/KMS 的 **hardware plane** 机制。现代 GPU 的显示引擎不只有一个 scanout buffer，而是支持多个 plane，每个 plane 可以指向不同的 framebuffer，由显示控制器在硬件层面完成最终合成。

关键点是：只要把应用的 buffer 绑定到某个 plane（最常见是 primary plane），显示控制器就能直接读取这块内存完成扫描输出，从而绕过 GPU 合成。

> 说明：在 Wayland 下，`wl_buffer` 的底层可能是 `wl_shm` 或 `linux-dmabuf`。只有能被 KMS plane 接受的格式/修饰符，才有机会走 direct scan-out。

## wlroots 中的实现：条件检查

wlroots 的 scene graph 在 `wlr_scene_output_build_state` 内部会尝试 direct scan-out，但成功的前提是 **一系列严格条件同时满足**。

### 1. 场景树只有一个可见节点

如果屏幕上有多个可见窗口，primary plane 只能指向一个 buffer，其余只能通过合成画进去——除非利用 overlay plane 同时 scan-out 多个 buffer（条件更严苛，且 wlroots 目前主要以 primary plane 单 buffer 为主）。

典型的成功场景是：全屏游戏、全屏视频播放器独占屏幕。

### 2. DRM 格式 / modifier 匹配

应用渲染时选择的像素格式（如 `DRM_FORMAT_XRGB8888`）和 tiling modifier（如 `I915_FORMAT_MOD_X_TILED`）必须出现在 KMS plane 的支持列表里。内核通过 `drmModeGetPlaneResources` 和 `DRM_IOCTL_MODE_OBJ_GETPROPERTIES` 暴露每个 plane 支持的 format/modifier 组合，wlroots 会在初始化时读取这张表并做匹配。

### 3. 无缩放、全屏对齐

KMS plane 的 `SRC_*` 和 `CRTC_*` 需要一一对应，不能要求缩放。如果应用 buffer 的分辨率与输出不一致（例如 letterbox），或在输出内有偏移，就需要 GPU 进行 blit/缩放，scan-out 不可行。

### 4. 变换（transform）匹配

屏幕旋转（例如 90°）时，如果应用的 buffer 未包含对应旋转，KMS 需要支持硬件旋转（`DRM_MODE_ROTATE_*`）才能 scan-out；否则只能先由 GPU 旋转再扫描输出。

## wlroots 的自动回退机制

wlroots 使用 DRM **atomic commit** 的 `DRM_MODE_ATOMIC_TEST_ONLY` 先做一次探测：

```c
// 伪代码描述 wlroots 内部逻辑
int ret = drmModeAtomicCommit(fd, req, DRM_MODE_ATOMIC_TEST_ONLY, NULL);
if (ret == 0) {
    // 硬件接受这个配置，正式提交 scan-out
    drmModeAtomicCommit(fd, req, DRM_MODE_PAGE_FLIP_EVENT, NULL);
} else {
    // 驱动拒绝（通常是 EINVAL），静默回退到 GPU 合成
    fallback_to_gpu_composite();
}
```

这意味着回退是自动且无损的：compositor 不需要显式区分底层走了哪条路径。

## 实际效益

Direct scan-out 的收益因硬件与场景而异，但常见改善包括：

- **全屏游戏**：减少一次合成渲染，降低帧延迟和带宽占用。
- **全屏视频播放**：减少中间合成带来的额外读写，CPU/GPU 负载更低。
- **单显示器独占窗口**：显示引擎直接扫描，功耗通常会更好。

对于使用 wlroots scene graph 的 compositor（例如 labwc、sway 的部分开发版本），direct scan-out 往往是“免费优化”：只要硬件与场景满足条件，wlroots 会自动启用它。
