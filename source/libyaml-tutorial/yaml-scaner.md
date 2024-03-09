---
title: YAML 文档扫描
lang: zh-CN
date: 2024 年 03 月 06 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

LibYAML 库基于事件发送机制将 C 数据结构转换为 YAML 文档。它将 YAML 文档解析为 C 数据结构的过程，也是基于类似的事件模型，只是将事件由发送变为析取。本文实现了一些简单的 YAML 文档扫描器，用于大致理解 LibYAML 基于事件的 YAML 文档解析过程。

# 空文档

假设 foo.yaml，其内容为

```yaml
---
...
```

以下 C 程序

```C
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
        FILE *file = fopen("foo.yaml", "r");
        yaml_parser_t parser;
        yaml_event_t event;
        yaml_event_type_t event_type;
        yaml_parser_initialize(&parser);
        yaml_parser_set_input_file(&parser, file);
        do {
                if (!yaml_parser_parse(&parser, &event)) goto error;
                switch (event.type) {
                case YAML_STREAM_START_EVENT:
                        printf("stream-start-event (%d)\n", event.type);
                        break;
                case YAML_STREAM_END_EVENT:
                        printf("stream-end-event (%d)\n", event.type);
                        break;
                case YAML_DOCUMENT_START_EVENT:
                        printf("document-start-event (%d)\n", event.type);
                        break;
                case YAML_DOCUMENT_END_EVENT:
                        printf("document-end-event (%d)\n", event.type);
                        break;
                }
                event_type = event.type;
                yaml_event_delete(&event);
        } while (event_type != YAML_STREAM_END_EVENT);
        yaml_parser_delete(&parser);
        fclose(file);
        return EXIT_SUCCESS;
error:
        fprintf(stderr, "Failed to parse: %s\n", parser.problem);
        yaml_parser_delete(&parser);
        fclose(file);
        return EXIT_FAILURE;
}
```

输出为

```text
stream-start-event (1)
document-start-event (3)
document-end-event (4)
stream-end-event (2)
```

# 映射

假设 foo.yaml 内容为

```yaml
hi: Hello world!
```

在上一节 C 程序的 `switch...case` 结构中增加以下代码便可捕获映射事件：

```c
case YAML_MAPPING_START_EVENT:
        printf("mapping start event (%d)\n", event.type);
        break;
case YAML_MAPPING_END_EVENT:
        printf("mapping end event (%d)\n", event.type);
        break;
case YAML_SCALAR_EVENT:
        printf("scalar event (%d)\n", event.type);
        break;
```

程序输出为

```text
stream start event (1)
document start event (3)
mapping start event (9)
scalar event (6)
scalar event (6)
mapping end event (10)
document end event (4)
stream end event (2)
```

# 标量

LibYAML 析取的标量事件包含着字符串形式的标量值及其长度。以下 C 代码可吸取上一节示例中的映射：

```c
case YAML_MAPPING_START_EVENT:
        printf("mapping start event (%d)\n", event.type);
        break;
case YAML_MAPPING_END_EVENT:
        printf("mapping end event (%d)\n", event.type);
        break;
case YAML_SCALAR_EVENT:
        printf("scalar event (%d) = {value = \"%s\", length = %d}\n",
               event.type,
               STR(event.data.scalar.value),
               (int)(event.data.scalar.length));
        break;
```

其中 `STR` 为宏，其定义为

```C
#define STR(x) ((x) ? (char *)(x) : "")
```

输出为

```text
mapping start event (9)
scalar event (6) = {value = "hi", length = 2}
scalar event (6) = {value = "Hello world!", length = 12}
mapping end event (10)
```

# 序列

假设 foo.yaml 内容为

```yaml
- a
- b
- c
```

在第一节「空文档」中 C 程序的 `switch...case` 代码中增加以下代码便可扫描上述序列：

```c
case YAML_SEQUENCE_START_EVENT:
        printf("sequence start event (%d)\n", event.type);
        break;
case YAML_SEQUENCE_END_EVENT:
        printf("sequence end event (%d)\n", event.type);
        break;
case YAML_SCALAR_EVENT:
        printf("scalar event (%d)\n", event.type);
        break;
```

程序输出结果为

```text
stream start event (1)
document start event (3)
sequence start event (7)
scalar event (6)
scalar event (6)
scalar event (6)
sequence end event (8)
document end event (4)
stream end event (2)
```

