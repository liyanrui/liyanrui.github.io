---
title: YAML 简介
lang: zh-CN
date: 2024 年 03 月 02 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

对于大多数工具，我们通常有八成的机会用到它二成的功能。YAML 应该也遵守这样的二八法则。

# 文档起止

一份规范的 YAML 文档，从三个「`-`」 开始：

```YAML
---
```

结束于三个「`.`」：

```YAML
...
```

对于一份单独的 YAML 文档，即其内容皆为 YAML 标记，则文档的起止标记并非必要，但多份 YAML 文档存于一份文件中，则文档起始标记 `---` 可作为文档分割符使用，例如

```YAML
---
name: 文档 1
---
name: 文档 2
---
name: 文档 3
```

YAML 文档结束标记也并非必要，但是在一份文件（流）中，若 YAML 文档之后还有其他非 YAML 文档内容，则文档结束标记可使得 YAML 解析器适时终止工作。例如下面提供给 pandoc 程序的输入文件，其首部嵌入了一份 YAML 文档：

```YAML
---
title: YAML 简介
date: 2024 年 03 月 02 日
...

对于一个工具，通常我们只需要学习它的 20% 的功能，便可以获得它 80% 的价值了，YAML 并不例外……

```

# 映射

在上一节最后一个示例中，YAML 文档的内容除了起止标记，剩下的内容是两个映射：

```yaml
title: YAML 简介
date: 2024 年 03 月 02 日
```

在 YAML 中，映射即键值对，冒号的左侧为键，右侧为值，且冒号与值之间至少存在一个空格。

# 标量

YAML 的标量有空值、布尔值、整型数、浮点数、日期、时间、字符串，它们皆可作为映射中的值，以与编程语言里的基本类型对应，例如

```YAML
parent: ~ # 或 null，空值
display: true # 或 false，布尔值
width: 600 # 整型数
height: 400 # 整型数
weight: 0.9 # 浮点数
date: 2024-03-03 # 日期，iso8601 格式
time: 2024-03-03T07:36+08:00 # 时间，iso8601 格式
title: 这是一个窗口 # 字符串
```

在 YAML 文档中，「`#`」及其之后的内容是注释。

# 字符串

字符串标量较其他标量复杂一些，故专门开设一节予以探讨。

一般情况下，字符串不需要使用引号包围。例如

```YAML
foo: 我能吞下玻璃而不伤身体
```

若字符串中包含特殊符号，则该字符串需要用引号包围。例如

```YAML
foo: 'width: 300'
```

或

```YAML
foo: "width: 300"
```

字符串可以呈多行，但是从第二行开始必须至少缩进一个空格，且换行符会被转为空格。例如

```YAML
foo: 这是多行
 字符串
```

YAML 解析器可将 `foo` 对应的值解析为

```YAML
这是多行 字符串
```

多行字符串可使用「`|`」 符号保留换行符，也可以使用「`>`」符号消除换行符。例如

```yaml
foo: |
  这是多行
  字符串
bar: >
  这是多行
  字符串
```

`foo` 和 `bar` 的值可分别被解析为

```yaml
这是多行\n字符串\n
这是多行 字符串\n
```

其中「`\n`」表示换行符。

「`+`」可用于保留多行字符串末尾的换行符，「`-`」可用于删除字符串末尾的换行符。例如

```YAML
---
foo-1: |
  这是多行
  字符串
  
foo-2: |+
  这是多行
  字符串
  
foo-3: |-
  这是多行
  字符串
  
...
```


上述三个标量的值可分别被解析为

```yaml
这是多行\n字符串\n
这是多行\n字符串\n\n
这是多行\n字符串
```

# 序列

一组以「`-`」和空格开头的行构成一个序列。

一组标量可构成序列：

```yaml
- 1
- 2
- 3
```

YAML 解析器可将上例解析为数组

```yaml
[1, 2, 3]
```

一组映射也可以构成序列。例如

```yaml
- foo: hello foo!
- bar: hello bar!
```

YAML 解析器可将其解析为

```python
[{'foo': 'hello foo!'}, {'bar': 'hello bar!'}]
```

序列也可以构成映射。例如

```yaml
foo:
  - 1
  - 2
  - 3
```

解析结果为

```yaml
{'foo': [1, 2, 3]}
```
