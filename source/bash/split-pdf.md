---
title: PDF 文件拆分
lang: zh-CN
date: 2023 年 07 月 06 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

对于 Kindle 设备，对 PDF 文件进行拆分尤为需要。Kindle 支持以电子邮件的方式向设备发送电子书，而扫描版的 PDF 文件通常超出邮箱附件容量限制（通常不超过 50 MB），需要将其分割为一组小文件方可作为邮件附件发送给设备。

Linux 环境中的 PDF 解析库 poppler（大多数 Linux 发行版自带）提供了一些处理 PDF 文件的程序，其中 pdfseparate 和 pdfunite 可用于拆分 PDF 文件。pdfseparate 可抽取 PDF 文件的每一页保存为单独的 PDF 文件。pdfunit 将指定的一组 PDF 单页文件合并为一份 PDF 文件。

假设待分割的 PDF 文件为 demo.pdf，首先使用 pdfseparate 对其进行拆分：

```bash
$ pdfseparate demo.pdf demo-page-%d.pdf
```

上述命令可在当前目录生成 demo-page-1.pdf, demo-page-2.pdf...单页文件名的 `%d` 是页码通配符。

对于生成的单页文件可使用以下命令进行排序：

```bash
$ ls demo-page-*.pdf | sort -V
```

将上述命令与 pdfunite 结合，便可 PDF 单页文件合并为一份 PDF 文件：

```bash
$ pdfunite $(ls demo-page-*.pdf | sort -V) output.pdf
```

一旦确定将目标 PDF 文件分割为几份，便可将 pdfseparate 生成的单页文件，按照文件名所含的页码将其复制到单独的目录。文件分割几份，便构建相应数量的存放单页文件的目录，然后在每个目录下使用 pdfunite 生成分割结果。该过程可以通过 Bash 脚本实现自动化，不过倘若不需要对大量文件进行分割，使用上述命令手动分割也不甚费时。
