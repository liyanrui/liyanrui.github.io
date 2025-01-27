---
title: C 数据结构 -> YAML
lang: zh-CN
date: 2024 年 03 月 03 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

若一个 C 程序需要向其他程序传递数据，基于 YAML 文档是可行的。C 语言的数据类型通常能够转换为等价的 YAML 格式。LibYAML 库提供了一组事件发送函数，可将 C 语言数据结构保存为 YAML 文档。

在实践前，确认系统已安装了 libYAML。在 Debian 或 Ubuntu 类的系统中，可使用以下命令安装：

```console
$ sudo apt install libyaml-dev
```
# 空文档

遇到复杂的事物，不妨将其输出设置为空，看看它的运作方式及结果。以下 C 程序仅能输出一份空的 YAML 文档：

```C
#include <stdio.h>
#include <yaml.h>

int main(void)
{
        /* 构造事件发送器 */
        yaml_emitter_t emitter;
        FILE *f = fopen("output.yaml", "w");
        if (!f) {
                fprintf(stderr, "Failed to open file!");
                exit(EXIT_FAILURE);
        }
        yaml_emitter_initialize(&emitter);
        yaml_emitter_set_output_file(&emitter, f);

        /* 发送 YAML 流和文档的开始事件 */
        yaml_event_t event;
        yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
        if (!yaml_emitter_emit(&emitter, &event)) goto error;
        yaml_document_start_event_initialize(&event, NULL, NULL, NULL, 0);
        if (!yaml_emitter_emit(&emitter, &event)) goto error;
        
        /* 待加入的代码 */
        
        /* 发送 YAML 文档和流的结束事件 */
        yaml_document_end_event_initialize(&event, 0);
        if (!yaml_emitter_emit(&emitter, &event)) goto error;
        yaml_stream_end_event_initialize(&event);
        if (!yaml_emitter_emit(&emitter, &event)) goto error;
        
        /* 释放资源并退出程序 */
        yaml_event_delete(&event);
        yaml_emitter_delete(&emitter);
        fclose(f);
        return EXIT_SUCCESS;

error: /* 错误处理 */
       fprintf(stderr, "事件 %d 发送失败：%s\n", event.type, emitter.problem);
       yaml_event_delete(&event);
       yaml_emitter_delete(&emitter);
       fclose(f);
       return EXIT_FAILURE;
}
```

输出一份空的 YAML 文档居然需要如此繁冗的代码，此举有些癫狂。不过，在尚未对 LibYAML 有所了解的情况下，暂且容忍，毕竟 C 语言的 YAML 解析库没有太多选择。

假设将上述示例代码保存于 foo.c 文件，可使用以下命令予以编译和运行：

```console
$ gcc foo.c -o foo -lyaml
$ ./foo
事件 4 发送失败：expected SCALAR, SEQUENCE-START, MAPPING-START, or ALIAS
```

程序运行结果错误，通过一番排查，可确定是以下两行代码所致：

```c
yaml_document_end_event_initialize(&event, 0);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
```

根据程序的出错信息猜测是因为输出了无内容的 YAML 文档，亦即 LibYAML 的事件发送函数在发送 `yaml_document_end_event` 时会检测 YAML 文档是否包含有效内容。

基于上述观察，大致可了解 LibYAML 是以发送事件的方式向 YAML 文档中传入数据，并在事件发送过程中检测数据的有效性。也能看到，每个事件皆需要一个初始化操作，然后发送。

# Hello world!

将 C 语言的一个字符串对象保存为 YAML 格式。例如将

```C
char *hi = "Hello world!";
```

保存为

```YAML
---
hi: Hello world!
...
```

LibYAML 为普通对象提供的发送事件是 `mapping`（映射）和 `scalar`（标量），以下为具体的事件发送代码：

```c
/* 发送映射开始事件 */
yaml_mapping_start_event_initialize(&event, NULL,
                                    YAML_MAP_TAG, 1, YAML_ANY_MAPPING_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;

/* 发送两个标量事件，将 hi 字符串对象写入 YAML 流 */
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "hi", strlen("hi"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             hi, strlen(hi), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;

/* 发送映射结束事件 */
yaml_mapping_end_event_initialize(&event);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
```

将上述代码添加到上一节示例中的「待加入的代码」所在区域，编译该示例并执行，结果为

```YAML
---
hi: Hello world!
...
```

由上例可见，在发送映射事件时，每个对象的名字和值皆以标量的形式进行发送。

在一个映射事件的发送过程中，可以发送多个对象。例如发送两个对象：

```c
/* 发送映射开始事件 */
yaml_mapping_start_event_initialize(&event, NULL,
                                    YAML_MAP_TAG, 1, YAML_ANY_MAPPING_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;

/* 发送两个两个对象 */
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "hi", strlen("hi"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             hi, strlen(hi), 1, 0, YAML_LITERAL_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "foo", strlen("foo"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "FOO", strlen("FOO"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;

/* 发送映射结束事件 */
yaml_mapping_end_event_initialize(&event);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
```

结果为

```yaml
---
hi: Hello world!
foo: FOO
...
```

# 结构体

下面是一个简单的 C 结构体：

```c
struct {
        int id;
        char *name;
} foo;
foo.id = 3;
foo.name = "Foo";
```

与该结构体对应的 YAML 映射需要两层结构，其事件发送代码如下：

```c
/* 第一层映射开始 */
yaml_mapping_start_event_initialize(&event, NULL,
                                    YAML_MAP_TAG, 1, YAML_ANY_MAPPING_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 结构体的名字 */
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "foo", strlen("foo"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 第二层映射开始 */
yaml_mapping_start_event_initialize(&event, NULL,
                                    YAML_MAP_TAG, 1, YAML_ANY_MAPPING_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 结构体成员 */
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "id", strlen("id"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
char foo_id[256]; sprintf(foo_id, "%d", foo.id);
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             foo_id, strlen(foo_id), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             "name", strlen("name"), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                             foo.name, strlen(foo.name), 1, 0, YAML_PLAIN_SCALAR_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 第二层映射结束 */
yaml_mapping_end_event_initialize(&event);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 第一层映射结束 */
yaml_mapping_end_event_initialize(&event);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
```

输出结果为

```yaml
---
foo:
  id: 3
  name: Foo
...
```

# 数组


假设有个数组

```c
int a[] = {1, 2, 3};
```

以下代码可将其保存为 YAML 序列：

```c
/* 序列开始 */
yaml_sequence_start_event_initialize(&event, NULL, YAML_SEQ_TAG,
                                     1, YAML_ANY_SEQUENCE_STYLE);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
/* 发送数组元素 */
for (size_t i = 0; i < 3; i++) {
        char a_i[256];
        sprintf(a_i, "%d", a[i]);
        yaml_scalar_event_initialize(&event, NULL, YAML_STR_TAG,
                                     a_i, strlen(a_i), 1, 0, YAML_PLAIN_SCALAR_STYLE);
        if (!yaml_emitter_emit(&emitter, &event)) goto error;
}
/* 序列结束 */
yaml_sequence_end_event_initialize(&event);
if (!yaml_emitter_emit(&emitter, &event)) goto error;
```

输出结果为

```YAML
---
- 1
- 2
- 3
...
```

# 结论

关于将 C 数据结构输出为 YAML 文档之事，多数情况下还是直接手工格式化输出为好。例如输出数组：

```c
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
        int a[] = {1, 2, 3};
        FILE *f = fopen("output.yaml", "w");
        if (!f) {
                fprintf(stderr, "Failed to open file!\n");
                exit(EXIT_FAILURE);
        }
        fprintf(f, "---\n");
        for (size_t i = 0; i < 3; i++) {
                fprintf(f, "- %d\n", a[i]);
        }
        fprintf(f, "...\n");
        fclose(f);
        return EXIT_SUCCESS;
}
```
