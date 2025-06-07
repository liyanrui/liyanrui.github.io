---
title: lmd 脚本
homeback: ../../index.html
lang: zh-Hans
date: 2023 年 04 月 19 日
abstract: 一份 Bash 脚本，用于撰写和管理 Markdown 格式的文档并通过 [Pandoc](https://www.pandoc.org) 将其转换为 HTML 格式。
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

lmd 是一份 Bash 脚本，用于撰写和管理 Markdown 格式的文档并通过 [Pandoc](https://www.pandoc.org) 将其转换为 HTML 格式。

除 Bash 和 Pandoc 之外，lmd 还依赖以下工具：

* tree：以树状结构显示目录；
* realpath：用于获取文件或目录的绝对路径，属于 coreutils 包；
* sed 和 awk：用于解析和处理文本文件。

欲用 lmd，请确认系统中是否已包含上述工具。

# 获取与安装

可通过 git 获取 lmd：

```bash
$ git clone https://github.com/liyanrui/lmd.git
```

亦可下载 [lmd-main.zip](https://github.com/liyanrui/lmd/archive/refs/heads/main.zip) 包，解包后，得到目录 lmd-main，更其名为 lmd（当然，并非必须）。

将包含 lmd 脚本的目录移动到你觉得适合它的目录，例如 `/opt`，便可完成 lmd 的安装，可使用 `tree` 命令查看其目录结构：

```bash
$ tree /opt/lmd
/opt/lmd
├── data
│   ├── appearance
│   │   ├── lmd.css
│   │   └── pandoc
│   │       └── data
│   │           └── templates
│   │               ├── homepage.template
│   │               └── post.template
│   └── lmd.conf
├── helper
│   ├── abstract.awk
│   ├── appear.awk
│   ├── hide.awk
│   └── title.awk
└── lmd

6 directories, 9 files
```

然后，将 lmd 脚本所在路径添加至 `PATH` 变量，亦即在 `$HOME/.bashrc` 中增加以下内容：

```bash
export PATH=/opt/lmd:$PATH
```

无论 lmd 脚本被安装至何处，只需要保证它与 data 以及 helper 目录位于同一目录即可。

# 创建文集

使用 `lmd init` 命令可创建一个目录，作为文集的根目录。该命令的用法为

```bash
$ lmd init "文集名" 文集目录
```

例如在 `$HOME/documents` 目录里创建文集根目录：

```bash
$ cd $HOME/documents
$ lmd init "月下磨刀集" liyanrui.github.io
```

在文集根目录或其任一子目录中，使用 `lmd tree` 命令可查看文集目录结构。例如

```bash
$ cd liyanrui.github.io
$ lmd tree
liyanrui.github.io
├── appearance
│   ├── lmd.css
│   └── pandoc
│       └── data
│           └── templates
│               ├── homepage.template
│               └── post.template
├── figures
├── index.md
├── lmd.conf
├── output
└── source

7 directories, 5 files
```

文集根目录中的 lmd.conf 文件不可删除，因为在文集的子目录中，lmd 需要以该文件为标识确定文集根目录的相对路径。lmd.conf 文件的内容可根据自己的情况进行修改。

# 首页

文集根目录中的 index.md 文件用于使用 Markdown 标记语言撰写文集首页内容。使用 `lmd view` 命令可将其转换为 index.html 并使用 lmd.conf 文件中设定的浏览器程序查看结果，例如

```bash
$ lmd view index.md
```

倘若只是将 index.md 转换为 index.html，不需要查看结果，可使用 `lmd convert` 命令：

```bash
$ lmd convert index.md
```

这两个命令同样可以用于“[创建和删除文章](#创建和删除文章)”一节中创建的文章。

# 创建/删除文章分类

除了首页，所有的 Markdown 文件皆应放在文集根目录下的 `source` 目录——简称为文集 source 目录。为了便于管理，可在 source 目录下创建一些子目录作为文章分类。使用 `mkdir` 命令可以完成该任务，但是需要执行三次，例如创建一个文章分类 2023：

```bash
$ cd source
$ mkdir 2023
$ cd ../figures
$ mkdir 2023
$ cd ../output
$ mkdir 2023
$ cd ../source
```

原因是，文集根目录下的 figures 和 output 目录（分别简称为文集 figures 目录和文集 output 目录）的子目录结构必须与 source 相同，前者用于存放 source 目录中的 Markdown 文件里所用的插图，后者用于存放 Markdown 文件的输出结果。

`lmd new category` 命令与上述的 `mkdir`等效，简化了文章分类的创建过程。例如在当前目录下创建文章分类：

```bash
$ cd source
$ lmd new category "2023 年" 2023
```

上述命令可创建目录 2023 且在该目录内创建 2023.md 文件，后者可用于制作分类文章目录，详见后文「制作文集目录」一节。

`lmd delete category` 命令可删除当前目录下的文章分类目录，例如

```bash
$ lmd delete category 2023
```

# 创建和删除文章

在文章分类目录，使用 `lmd new post` 命令创建 Markdown 文件。例如创建标题为「lmd 脚本」的 Markdown 文件 foo.md：

```
$ cd 2023
$ lmd new post "文章标题" foo.md
```

注意，所创建的 Markdown 文件，其扩展名必须为「.md」。

上述命令不仅在 source/2023 目录创建了 Markdown 文件 foo.md，也在 figures/2023 目录创建了目录 foo，用于表明该目录中所有图片皆作为 foo.md 中的插图。此外，该命令会根据 lmd.conf 中设定的文本编辑器打开 foo.md。

新建的 Markdown 文件，已经填好了一些元信息。例如上述命令创建的 foo.md，其初始内容为

```Markdown
---
title: 文章标题
homeback: ../../index.html
lang: zh-CN
date: 2023 年 04 月 19 日
footer: 若需要联系我，可发邮件至 <lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...
```

其中 `date` 的格式以及 `footer` 的内容可在 lmd.conf 中修改 `DATE` 和 `FOOTER` 的值进行定制，`lang` 可在 Markdown 文件中根据需要自行修改。例如，倘若文章是英文内容，可将 `zh-CN` 修改为 `en` 或 `en-US`。`homeback` 用于设定首页路径，因为任何一篇接受 lmd 脚本管理的文章，其 HTML 页面会有一个超级链接，指向文集首页。文章的元信息是提供给 pandoc 的，以备其生成 HTML 页面。

`lmd delete post` 用于删除当前文章分类目录下的 Markdown 文件，例如

```bash
$ lmd delete post foo.md
```

该命令也会删除文集的 figures 和 output 目录中与 foo.md 相关的子目录。

# 重命名分类和文章

`lmd rename category` 和 `lmd rename post` 分别用于对当前目录下的分类和 Makdown 文件重新命名。例如，将分类目录 2023 更名为 notes：

```bash
$ lmd rename category 2023 notes
```

将 foo.md 更名为 bar.md：

```bash
$ lmd rename post foo.md bar.md
```

上述命令也会自动对文集的 figures 和 output 目录里相应的子目录进行更名。 

# 内容发布

与生成首页 HTML 类似，对于当前目录下的文章（Markdown 文件），使用 `lmd view` 或 `lmd convert` 将其转换为 HTML 文件，并存放于文集 output 目录内。

若你拥有一个支持部署个人网站的网络空间，只需将域名指向文集根目录便可将其发布于网络。倘若使用 github pages 服务，将文集根目录作为仓库目录提交至 github 即可。例如，对于 github 用户 foo，需要他先在 github 上创建一个空仓库 foo.github.io，然后执行以下命令

```bash
$ git clone git@github.com:foo/foo.github.io
$ cd foo.github.io
$ cp -r 你的文集根目录/* ./
$ git commit -a
$ git commit -m "文集初次发布"
$ git push
```

日后只需要按照前文所述的 lmd 命令，便可持续创建文章，撰写内容，然后使用 `git push` 发布文章。

# 制作文集目录

文集目录可以手动编辑生成，只要在目录页面添加指向某篇文章的链接即可，例如可在文集根目录下的 index.md 文件中以列表的形式添加某篇文章：

```markdown
* [lmd 脚本](output/2023/lmd.html)：一份 Bash 脚本，用于撰写和管理 Markdown 格式的文档并通过 [Pandoc](https://www.pandoc.org) 将其转换为 HTML 格式。
```

lmd 脚本可将上述手动过程半自动化，前提是文章的元信息区域需提供 abstract（摘要），例如

```markdown
---
title: lmd 脚本
homeback: ../../index.html
lang: zh-CN
date: 2023 年 04 月 19 日
abstract: 一份 Bash 脚本，用于撰写和管理 Markdown 格式的文档并通过 [Pandoc](https://www.pandoc.org) 将其转换为 HTML 格式。
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...
```

假设文章对应的 Markdown 文件为 lmd.md，可使用以下命令在 lmd.md 所在目录将文章名称、链接以及摘要以列表项的形式添加到文集根目录下的 index.md：

```bash
$ lmd appear lmd.md $(lmd root)/index.md
```

`lmd root` 命令可获得当前目录至文集根目录的相对路径。在指定目录文件中增加的文章信息总是位于目录的顶端，倘若其后有其重复条目，lmd 脚本会自动予以忽略。

若想在目录文件中去掉某篇文章，可在该文章所在目录下执行 `lmd hide` 命令。例如

```bash
$ lmd hide lmd.md $(lmd root)/index.md
```

# 外观

在文集的 appearance 目录里，lmd.css 用于定制 HTML 页面元素的外观。在 appearance/pandoc/data/templates 目录中有两份 pandoc 模板，homepage.template 和 post.template，前者为首页模板，后者为文章模板，若你熟悉 pandoc 的用法或者据其内容略加揣测，自行对页面应当出现哪些元素予以定制。
