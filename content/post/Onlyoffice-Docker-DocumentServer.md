---
title: "Onlyoffice Document Server 搭建"
date: "2021-08-23"
math: false
tags:  ["onlyoffice"]
---

**Document Server** 是一个在线办公套件，包括用于文本，电子表格和演示文稿的查看器和编辑器，与Office Open XML格式完全兼容：.docx，.xlsx，.pptx，并支持实时协作编辑。

支持所有流行的格式：DOC，DOCX，TXT，ODT，RTF，ODP，EPUB，ODS，XLS，XLSX，CSV，PPTX，HTML

项目地址是：https://github.com/ONLYOFFICE

<!--more-->

### Docker-DocumentServer 的使用

参考：https://github.com/ONLYOFFICE/Docker-DocumentServer

```bash
sudo docker run -i -t -d -p 80:80 onlyoffice/documentserver
```

打开网址 http:127.0.0.1:80

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/2f294d0d73c0861a5de617420b26eaf8.png)

运行下面的命令

```bash
sudo docker exec 02bc8d53772d sudo supervisorctl start ds:example
```

打开 http://127.0.0.1/example/， 可以体验试用

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/c9fc220a96639f6afd0e06211d2e55c9.png)

> 如果不想用Docker，参考： [在Debian，Ubuntu和衍生产品上安装ONLYOFFICE Linux集成版](http://onlyoffice.cc/1155/) 如果需要配置https，上面教程中也有提到。

### 文件服务器

如果需要访问本地文件，需要配置一下 http 服务，这里以 http.server 模块为例。

```bash
python3 -m http.server -d "/home/rewine" -b localhost 8000
```

参数中 -d 指定目录， -b 绑定ip地址，完成后可以在浏览器中查看

### Docker容器访问宿主机

最简单的方法是使用host模式，不过缺点是降低了 docker 的隔离性。

而且这次需要手动执行一下 [run-document-server.sh](http://run-document-server.sh)。

```bash
$ sudo docker run -i -t -d --net=host onlyoffice/documentserver
$ sudo docker ps
CONTAINER ID   IMAGE                       COMMAND                  CREATED      STATUS      PORTS     NAMES
02bc8d53772d   onlyoffice/documentserver   "/app/ds/run-documen…"   5 days ago   Up 5 days             priceless_hofstadter
$ sudo docker exec -i -t 02bc8d53772d /bin/bash
# cd /app/ds
# ./run-document-server.sh
```

其他方法参考：

https://www.jianshu.com/p/4a358a120983

https://nyan.im/posts/3981.html

### 使用测试

下面是查看 text.doc 的小例子，可以用浏览器打开

```html
<!DOCTYPE html>
<html style="height: 100%;">

<head>
  <title>ONLYOFFICE Api Documentation</title>

<body style="height: 100%; margin: 0;">
  <div id="container">
    <div id="placeholder" style="height: 100%"></div>
  </div>
  <script type="text/javascript" src="<http://127.0.0.1/web-apps/apps/api/documents/api.js>"></script>
  <script type="text/javascript">
    window.docEditor = new DocsAPI.DocEditor("placeholder",
      {
        "documentType": "word",
        "height": "100%",
        "width": "100%",
        "type": "embedded",
        "document": {
          "fileType": "doc",
          "url": "<http://127.0.0.1:8000/text.doc>"
        }
      });
  </script>

</body>
</html>
```

具体用法参看教程：https://api.onlyoffice.com/editors/basic