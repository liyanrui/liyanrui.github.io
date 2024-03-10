---
title: YAML 解析
lang: zh-CN
date: 2024 年 03 月 08 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

假设存在以下 YAML 文档

```yaml
---
故乡:
  地名: 郯城
  经度: 118.374
  纬度: 34.619
...
```

基 LibYAML 库将上述 YAML 文档内容解析为 C 结构体

```c
struct {
        char *name;
        float longitude;
        float latitude;
} 
```

为了从 YAML 事件流中获取所需要的数据流，需要将 YAML 事件构成的状态机嵌入到针对上述 C 结构体而构造的状态机中，以下是完整的解析代码：

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STR(x) ((x) ? (char *)(x) : "")

int main(int argc, char *argv[])
{
        struct {
                char *name;
                float longtitude;
                float latitude;
        } hometown;
        enum hometown_state {
                INIT,
                MAYBE_HOMETOWN,
                HOMETOWN,
                HOMETOWN_NAME,
                HOMETOWN_LONGTITUDE,
                HOMETOWN_LATITUDE,
                TERMINAL
                
        } s = INIT;
        FILE *file = fopen("foo.yaml", "r");
        yaml_parser_t parser;
        yaml_event_t event;
        yaml_parser_initialize(&parser);
        yaml_parser_set_input_file(&parser, file);
        do {
                if (!yaml_parser_parse(&parser, &event)) goto error;
                switch (s) {
                case INIT:
                        switch (event.type) {
                        case YAML_MAPPING_START_EVENT:
                                s = MAYBE_HOMETOWN;
                                break;
                        }
                        break;
                case MAYBE_HOMETOWN:
                        switch (event.type) {
                        case YAML_SCALAR_EVENT:
                                char *value = STR(event.data.scalar.value);
                                if (strcmp(value, "故乡") == 0) {
                                        s = HOMETOWN;
                                }
                                break;
                        }
                        break;
                case HOMETOWN:
                        switch (event.type) {
                        case YAML_SCALAR_EVENT:
                                char *value = STR(event.data.scalar.value);
                                if (strcmp(value, "地名") == 0) s = HOMETOWN_NAME;
                                if (strcmp(value, "经度") == 0) s = HOMETOWN_LONGTITUDE;
                                if (strcmp(value, "纬度") == 0) s = HOMETOWN_LATITUDE;
                                break;
                        case YAML_MAPPING_END_EVENT:
                                s = TERMINAL;
                                break;
                        }
                        break;
                case HOMETOWN_NAME:
                        switch (event.type) {
                        case YAML_SCALAR_EVENT:
                                char *value = STR(event.data.scalar.value);
                                hometown.name = strdup(value);
                                s = HOMETOWN;
                                break;
                        }
                        break;
                case HOMETOWN_LONGTITUDE:
                        switch (event.type) {
                        case YAML_SCALAR_EVENT:
                                char *value = STR(event.data.scalar.value);
                                hometown.longtitude = atof(value);
                                s = HOMETOWN;
                                break;
                        }
                        break;
                case HOMETOWN_LATITUDE:
                        switch (event.type) {
                        case YAML_SCALAR_EVENT:
                                char *value = STR(event.data.scalar.value);
                                hometown.latitude = atof(value);
                                s = HOMETOWN;
                                break;
                        }
                        break;
                }
                yaml_event_delete(&event);
        } while (s != TERMINAL);
        yaml_parser_delete(&parser);
        fclose(file);
        printf("{%s, %.3f, %.3f}\n",
               hometown.name, hometown.longtitude, hometown.latitude);
        free(hometown.name);
        return EXIT_SUCCESS;
error:
        fprintf(stderr, "Failed to parse: %s\n", parser.problem);
        yaml_parser_delete(&parser);
        fclose(file);
        return EXIT_FAILURE;
}
```

程序的输出为

```text
{郯城, 118.374, 34.619}
```

若觉 LibYAML 解析 YAML 文档的过程过于繁琐，且所解析的 YAML 文档中规中矩，建议使用 LibCYAML 库，可参考「<https://github.com/tlsa/libcyaml/blob/main/docs/guide.md>」。
