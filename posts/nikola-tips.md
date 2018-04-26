<!--
.. title: Nikola 的一些琐碎知识
.. slug: nikola-tips
.. date: 2018-04-26 10:03:25 UTC+08:00
.. tags: nikola
.. category: 笔记
.. link: 
.. description: 
.. type: text
-->

Nikola 是我用了 3 年的静态网页生成工具，这个 Github 主页上的所有页面皆由它生成。它的安装与基本用法可参考《[Nikola 入门文档](https://www.getnikola.com/getting-started.html)》，对于 Linux 环境，建议在 virtualenv + python 3 环境中安装它。它的更详细的用法，见《[Nikola 手册](https://www.getnikola.com/handbook.html)》。本文仅记录我在使用 Nikola 过程中遇到的一些小问题以及不甚高明的对策，仅作备忘之用。

# 自定义首页

Nikola 会自动生成首页，将 10 近期的文章内容收集到一个页面，该页面对应的文件为 index.html。这份文件默认会被放到站点的根目录里，所以，它就成了站点的首页。我觉得 Nikola 这个功能很鸡肋，因为我想自己定义首页，而不是这种自动生成的东西。该怎么办呢？

首先，在 conf.py 文件里，将 `INDEX_PATH` 设为

```
INDEX_PATH = "blog"
```

这样，Nikola 就会将这个自动生成的 index.html 文件放到 `站点根目录/blog` 目录里，从而令它失去了担当站点首页的资格。理论上，也可以随便取个目录名来用，例如
·
```
INDEX_PATH = "obsolete"
```

用这种乾坤大挪移的办法，算是将这个鸡肋的站点首页「消灭」了。接下来，就是建立一个自定义的首页，以后手工来维护它。方法很简单，只需要在 `PAGES` 目录，任意建立一份源文件，将其 `slug` 设为 `index.html` 即可。

`PAGES` 目录，就是在 conf.py 文件中设定的目录。例如，我的设定是：

```
PAGES = (
    ("stories/*.md", "", "story.tmpl"),
)
```

这就意味着，我可以在 `stories` 目录建立 `slug` 设为 `index.html` 的源文件（至于它对应的文件名是什么，并不重要）。假设这份源文件是 Markdown 格式，那么所谓 `slug` 就是文件头中的 `slug` 条目，例如：

```
<!-- 
.. title: 
.. slug: index
-->
```

剩下的内容可自由定义。

当网站内容重新发布时，这份源文件就会被转化为网站根目录中的 index.html 文件。因为 Nikola 就是这样对待 `PAGES` 目录中的源文件的，会把它们「平铺」在网站的根目录里。


