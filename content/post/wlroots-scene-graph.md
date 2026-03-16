---
title: "简要介绍 wlroots 的 scene graph"
date: 2026-03-14
tags: ["Wayland", "wlroots", "Linux"]
draft: false
---

wlroots 的 scene graph 是 0.15 引入的高层渲染抽象，目标是让 compositor 开发者摆脱手动管理渲染循环、damage tracking 和输入命中测试的繁琐工作。下面从架构、节点类型、坐标系、渲染流程和实际用法等几个维度展开。

---

## 整体架构

scene graph 的核心思想是把“要渲染什么”和“如何渲染”解耦。Compositor 只需构建一棵节点树来描述场景，wlroots 负责把这棵树翻译成 GPU 命令并处理 damage。

---

![wlroots scene graph 架构示意](/images/wlroots-scene-graph/wlroots_scene_node_architecture.svg)

## 节点类型详解

### `wlr_scene_node` — 基础结构

所有节点类型都内嵌一个 `wlr_scene_node`，这是整个 scene graph 的基础类型。它保存：

- **`type`**：枚举值，区分 `WLR_SCENE_NODE_TREE`、`WLR_SCENE_NODE_RECT`、`WLR_SCENE_NODE_BUFFER`
- **`parent`**：指向父 `wlr_scene_tree`
- **`x`, `y`**：相对于父节点的偏移坐标（整数，单位为逻辑像素）
- **`enabled`**：布尔值，`false` 时节点及其所有子节点不参与渲染和输入命中测试
- **`data`**：用户自定义指针，常用来反向关联 compositor 自己的对象（如 window 结构体）

### `wlr_scene_tree` — 容器节点

`wlr_scene_tree` 是唯一能有子节点的类型，子节点存储在一个 `wl_list` 中，**子节点顺序决定 z-order**。它本身不产生任何像素，只是一个坐标变换和可见性的传递容器。

`wlr_scene.tree` 就是整棵场景树的根 `wlr_scene_tree`。

### `wlr_scene_rect` — 纯色矩形

最简单的叶节点，渲染一个指定颜色和尺寸的矩形。常见用途：窗口装饰边框、遮罩层、调试可视化。颜色以 `float[4]`（RGBA）表示。

### `wlr_scene_buffer` — GPU 缓冲叶节点

接受任意 `wlr_buffer`，将其内容渲染到场景中。支持：

- **不透明区域提示**（`opaque_region`）：优化 damage 合并
- **源矩形**（`src_box`）：纹理裁剪
- **目标尺寸**（`dst_width/height`）：缩放到指定大小
- **变换**（`transform`）：翻转/旋转
- **透明度/不透明区域**：`opacity` + `opaque_region` 影响合成与遮挡优化

### `wlr_scene_surface` — Wayland surface 的封装

这是 compositor 最常用的工具函数，本质上是对 `wlr_scene_buffer` 的封装，并监听 `wlr_surface` 的 `commit` 事件，自动把新提交的 `wlr_buffer` 更新到场景。它还会：

- 自动处理 `wl_surface_damage` 产生的精确 damage 区域
- 处理子 surface（subsurface）递归创建子节点
- 通过对应 helper（如 `wlr_scene_xdg_surface_create`、`wlr_scene_layer_surface_v1_create`）把这些 surface 纳入场景树，统一管理弹出菜单（popup）

---

## 坐标系

scene graph 使用**输出布局坐标**（output-layout coordinates），即全局逻辑坐标空间（通常通过 `wlr_scene_attach_output_layout()` 与 `wlr_output_layout` 同步）：

- 每个节点的绝对坐标 = 从根节点累加所有父节点 `(x, y)` 偏移
- 多显示器时，不同屏幕在同一坐标空间内偏移排布（与 `wlr_output_layout` 对应）
- **Scale 由 `wlr_scene_output` 在渲染时处理**，节点本身的坐标始终是逻辑像素，不感知 HiDPI scale factor

![scene graph 坐标系示意](/images/wlroots-scene-graph/scene_coordinate_system.svg)

---

## 渲染流程

`wlr_scene_output` 是场景和物理屏幕之间的桥梁。每个 `wlr_output` 创建对应的 `wlr_scene_output` 之后，渲染循环变得极为简单：

```c
// Compositor 渲染循环核心（使用 scene API 之后）
void on_output_frame(struct wl_listener *listener, void *data) {
    struct wlr_scene_output *scene_output = /* ... */;

    if (!wlr_scene_output_commit(scene_output, NULL)) {
        return;
    }

    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    wlr_scene_output_send_frame_done(scene_output, &now);
}
```

在 `wlr_scene_output_commit()` 内部会构建输出 state（调用 `wlr_scene_output_build_state()`），wlroots 会：

1. **遍历场景树**，按 z-order 收集所有与该 output 相交的可见节点
2. **计算 damage 区域**：合并上次以来所有节点移动、大小变化、buffer 更新产生的脏区域
3. **决定渲染策略**：若支持 direct scan-out（全屏单一 buffer），直接 scan-out 跳过合成步骤
4. **调用 renderer**：将需要重绘的 damage 区域用 `wlr_renderer` 渲染到 framebuffer
5. **提交帧**：通过 DRM/KMS 或 Wayland 子协议翻转到屏幕

---

## Damage 追踪机制

scene graph 内部维护了精细的 damage tracking，不需要 compositor 手动计算哪些区域需要重绘：

- **节点位移**：移动节点时，旧位置和新位置都会被标记为 damage
- **节点启用/禁用**：切换 `enabled` 时自动 damage 相关区域
- **buffer 更新**：`wlr_scene_surface` 监听 `wlr_surface.commit`，把 `wl_surface_damage` 映射到全局坐标
- **不透明区域优化**：通过 `opaque_region` 提示，下层被完全遮盖的区域不会被绘制

---

## 输入命中测试

scene graph 也可以用于输入路由。`wlr_scene_node_at` 接受全局坐标，返回该点最顶层的可见节点：

```c
double sx, sy;
struct wlr_scene_node *node = wlr_scene_node_at(
    &scene->tree.node, lx, ly, &sx, &sy
);
// sx, sy 是相对于命中节点的局部坐标
```

通常结合 `node->data` 指针反查对应的窗口对象，再决定键盘聚焦或指针授权。

---

## 典型 compositor 用法模式

一个完整窗口在场景树中的组织方式如下：

```txt
scene->tree
  └── wlr_scene_tree  (toplevel window, data = my_window*)
        ├── wlr_scene_rect       (title bar 背景色)
        ├── wlr_scene_buffer     (title bar 文字纹理)
        ├── wlr_scene_surface    (xdg_toplevel wl_surface)
        │     └── [wlroots 自动创建 subsurface 子树]
        └── wlr_scene_surface    (xdg_popup)
```

在这种模式下，移动窗口只需：

```c
wlr_scene_node_set_position(&window->scene_tree->node, new_x, new_y);
// damage、重绘、子节点跟随 —— 全部自动处理
```

---

## 与旧式手动渲染的对比

| 方面 | 手动 `wlr_renderer` | scene graph API |
|---|---|---|
| Damage 追踪 | 需自行计算 | 自动 |
| Z-order 管理 | 手动排序列表 | 树结构天然有序 |
| Direct scan-out | 需手动判断 | 自动尝试 |
| 输入命中测试 | 需手动遍历 surface | `wlr_scene_node_at` |
| 代码量 | 数百行渲染循环 | 十余行 |

scene graph 是 wlroots 鼓励的现代用法，Sway、labwc、wayfire 等主流 compositor 已经或正在迁移到这套 API。
