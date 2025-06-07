---
title: 新的 lmd 脚本的用法
date: 2025 年 01 月 27 日
abstract: 近日，重写了 lmd 脚本。新版本更为简单和易用。
...

近日，重写了 lmd 脚本，详见「[lmd 脚本的设计与实现](../best-lmd/index.html)」。新的 lmd 脚本比[它的上个版本](../../old-posts/output/bash/lmd.html)更为简单，我觉得也更为易用，本文介绍它的用法。

# 安装

可通过 git 获取 lmd：

```console
$ git clone https://github.com/liyanrui/lmd.git
```

亦可下载 lmd-main.zip 包，解包后，得到目录 lmd-main，更其名为 lmd（并非必须）。将包含 lmd 脚本的目录移动到你觉得适合它的目录，例如 /opt，便可完成 lmd 的安装。然后，将 lmd 脚本所在路径添加至 PATH 变量，亦即在 $HOME/.bashrc 中增加以下内容：

```bash
export PATH=/opt/lmd:$PATH
```

无论 lmd 脚本被安装至何处，只需要保证它与 data 以及 helper 目录位于同一目录即可。

需要注意，执行 lmd 脚本，需要系统中存在 Bash 环境和 Awk（gawk、mawk……），这对于任何一个 Linux 系统（包括其他类 Unix 系统，例如 macOS）而言，很容易得到。即使在 Windows 系统中，获得 Bash 和 Awk 也并非难事。

# 网站初始化

假设在 `$HOME/documents` 目录里创建网站根目录 foo，只需执行以下命令：

```console
$ cd $HOME/documents
$ lmd init "Foo" "this is foo." foo
```

命令 `lmd init` 接受 2 个参数，第一个参数是网站的标题，第二个参数是网站根目录名。

进入 foo 目录，使用 `lmd tree` 命令可查看网站初始状态：

```console
$ cd foo
$ lmd tree
foo
├── appearance
│   ├── lmd.css
│   └── pandoc
│       └── data
│           └── templates
│               ├── homepage.template
│               └── post.template
├── figures
├── index.md
└── lmd.conf
```

如果需要将网站发布为 github 主页，记得在网站根目录下添加一份空文件：

```console
$ touch .nojekyll
```

该文件可避免 github 平台误将网站视为由平台默认的静态网站生成器 jekyll 创建。

# 文章

在网站根目录里，可以创建一篇文章，它可以是真正的文章，也可以作为一个分类，令其包含一些其他文章。

例如，在 foo 目录下创建一篇名为「2025 年的日志」的文章，用于包含本年度所有的日志：

```console
$ lmd new "2025 年的日志" 2025
$ cd 2025
$ lmd build
```

`lmd new` 也接受 2 个参数，且参数的含义与 `lmd init` 的参数相同。`lmd build` 命令可将文章及其上级文章（在上例中，上级文章即网站首页）生成为网页。

在 `2025` 目录里，创建本年度的一篇日志：

```console
$ lmd new "Hello world" hello-world
$ cd hell-world
```

执行以下命令可为文章增加时间戳：

```console
$ lmd add timestamp
```

然后用你习惯的文本编辑器，编辑 hello-world 目录里的 index.md 文件，这份文件包含的便是文章内容。完成文章创作后，在 hello-world 目录里执行

```console
$ lmd build
```

便可将文章转换为网页。

要删除一篇文章，例如删除上述的 hello-world，只需转到包含 hello-world 的目录：

```console
$ cd ..    # 从当前的 hello-world 目录转到上级目录
$ lmd delete hello-world
```

# 外观

在文集的 appearance 目录里，lmd.css 用于定制 HTML 页面元素的外观。在 appearance/pandoc/data/templates 目录中有两份 pandoc 模板，homepage.template 和 post.template，前者为首页模板，后者为文章模板，若你熟悉 pandoc 的用法或者据其内容略加揣测，自行对页面应当出现哪些元素予以定制。
