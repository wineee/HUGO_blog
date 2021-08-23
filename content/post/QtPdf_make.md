---
title: "QtPDF 的编译与试用"
date: "2021-08-23"
math: false
tags:  ["Qt"]
---

QtPDF 是 Qt 封装的 pdfium 模块，以 [GPLv3/](https://www.gnu.org/licenses/gpl-3.0.html)[LGPLv3](https://www.gnu.org/licenses/lgpl-3.0.html) 协议分发，也可以在 QT marketplace 购买可商业版本

### 下载源代码

```bash
git clone <https://code.qt.io/qt/qtwebengine.git>
cd qtwebengine
git checkout 5.12.8
git submodule update --init --recursive
```

注意：git checkout 选择自己 qt 的版本

<!--more-->


### 安装构建依赖

```bash
sudo apt install bison build-essential gperf flex python2 libasound2-dev libcups2-dev libdrm-dev libegl1-mesa-dev libnss3-dev libpci-dev libpulse-dev libudev-dev nodejs libxtst-dev gyp ninja-build
sudo apt install libssl-dev libxcursor-dev libxcomposite-dev libxdamage-dev libxrandr-dev libfontconfig1-dev libxss-dev libsrtp2-dev libwebp-dev libjsoncpp-dev libopus-dev libminizip-dev libavutil-dev libavformat-dev libavcodec-dev libevent-dev libvpx-dev libsnappy-dev libre2-dev libprotobuf-dev protobuf-compile
```

如果报错 Project ERROR: Unknown module(s) in QT: qml-private， 需要安装 qtdeclarative5-dev

### 编译

```bash
cd ..
mkdir build 
cd build 
qmake ../qtwebengine
NINJAJOBS=-j7 make -j7 
make install
```

下面是 qmake 的输出信息，如果依赖安装不全可以参考一下

```bash
Running configuration tests...
Done running configuration tests.

Configure summary:

Qt WebEngine:
  Embedded build ......................... no
  Full debug information ................. no
  Pepper Plugins ......................... yes
  Printing and PDF ....................... yes
  Proprietary Codecs ..................... no
  Spellchecker ........................... yes
  Native Spellchecker .................... no
  WebRTC ................................. yes
  Use System Ninja ....................... no
  Geolocation ............................ yes
  WebChannel support ..................... yes
  Use v8 snapshot ........................ yes
  Kerberos Authentication ................ no
  Support qpa-xcb ........................ yes
  Use ALSA ............................... yes
  Use PulseAudio ......................... yes
  Optional system libraries used:
    re2 .................................. yes
    icu .................................. no
    libwebp, libwebpmux and libwebpdemux . yes
    opus ................................. yes
    ffmpeg ............................... no
    libvpx ............................... yes
    snappy ............................... yes
    glib ................................. yes
    zlib ................................. yes
    minizip .............................. yes
    libevent ............................. yes
    jsoncpp .............................. yes
    protobuf ............................. yes
    libxml2 and libxslt .................. yes
    lcms2 ................................ yes
    png .................................. yes
    JPEG ................................. yes
    harfbuzz ............................. yes
    freetype ............................. yes
  Required system libraries:
    fontconfig ........................... yes
    dbus ................................. yes
    nss .................................. yes
    khr .................................. yes
    glibc ................................ yes
  Required system libraries for qpa-xcb:
    x11 .................................. yes
    libdrm ............................... yes
    xcomposite ........................... yes
    xcursor .............................. yes
    xi ................................... yes
    xtst ................................. yes

Qt is now configured for building. Just run 'make'.
Once everything is built, you must run 'make install'.
Qt will be installed into '/usr'.

Prior to reconfiguration, make sure you remove any leftovers from
the previous build.
```

编译过程需要2-3个小时，内存占用高峰大概需要16G + 5G swap

### 使用测试

```makefile
QT += core gui widgets pdf pdfwidgets
```

如果能正常识别，说明配置成功了，可以试试下面这个例子：

https://github.com/CryFeiFei/QtPdfTest

[PDF Viewer Example](https://doc.qt.io/qt-5/qtpdf-pdfviewer-example.html)

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/2006288850634b247ab10c5c0c128982.png)

### 参考资料

- [QtPDF Build Instructions](https://wiki.qt.io/QtPDF_Build_Instructions)
- [New QtLabs PDF module](https://www.qt.io/blog/2017/01/30/new-qtpdf-qtlabs-module)
- [QtWebEngine/How to Try](https://wiki.qt.io/QtWebEngine/How_to_Try)