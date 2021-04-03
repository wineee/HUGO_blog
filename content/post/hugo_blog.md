+++

title = "搭建个人博客记录(github action+hugo)"
date = "2021-01-15"
description = "hugo blog"
tags = ["hugo"]

+++

### 前言

高中时，作为OIer经常需要看别人的题解博客，那时候luogu还没流行，大家都在tyvj，vijos上刷题，看题解还要找 csdn，博客园，和一些大佬（比如hzwer）的个人博客。慢慢的，就萌生出了自己写博客的想法，一开始，我在蚂蚁笔记上写题解，它可以把markdown笔记分享到蚂蚁博客上，而且主题也很美观，后来它收费了，学生党没有钱（那时候我没手机，没手机号，更别说网银了）弃用了，现在我不推荐大家用了，这个项目2，3年没人维护，恐怕不靠谱。

我还试过WordPress，用的免费主机（当然现在挂了，资料也丢了）

以前在同学帮助下搭建了 hexo+github 的 blog，不过我嫌主题不好看万年不更新。

[csdn](blog.csdn.net/qq_33831360) 的blog因为要绑定手机号，高中没机会用，大一才开始写一些东西。

[博客园](https://home.cnblogs.com/u/rewine/) 后台太丑，不喜欢用这个写。

[gridea](https://github.com/getgridea/gridea) 这个是我去年了解的，看上去挺方便的，我没试过，感兴趣的可以看看。

<!--more-->

emacs 搞成的博客也不错，比如：https://evanmeek.github.io

###  hugo 博客搭建

我现在用的就是 hugo， 本着不重复造轮子的原则，我只放一些参考的文章，不会详细写每个步骤。

#### 选择主题

在官网找一个顺眼的主题

https://themes.gohugo.io/

我的要求主要是简洁，选用了hugo主题

https://github.com/dsrkafuu/hugo-theme-fuji

#### 搭建 hugo 博客

这方面教程很多

https://zhuanlan.zhihu.com/p/105021100

[HuGo博客搭建部署到GitHub Pages](https://blog.csdn.net/qq_33831360/article/details/107484580)

#### 自动化部署（非必须）

> 方便用多台电脑是同步

##### [Hugo + Github Actions 实现自动化部署](https://immmmm.com/hugo-github-actions/)

如果失败，原因很可能是教程里配置版本太低了，可以参考我的配置

https://github.com/wineee/HUGO_blog/tree/master/.github/workflows



#### 其他参考资料

[Hugo中常用命令及参数](https://hugo.aiaide.com/post/hugo%E4%B8%AD%E5%B8%B8%E7%94%A8%E5%91%BD%E4%BB%A4%E5%8F%8A%E5%8F%82%E6%95%B0/)

[HuGo博客处理数学公式方案](https://blog.csdn.net/qq_33831360/article/details/107490524)



**工具：Typora 编写文章， Picgo 上传图片。**

