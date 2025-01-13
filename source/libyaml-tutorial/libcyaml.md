---
title: LibCYAML 库的用法
lang: zh-CN
date: 2024 年 03 月 11 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

虽然使用 LibYAML 能够实现几乎全部的 YAML 文档解析任务，但当 YAML 文档较为复杂时，解析代码颇为繁冗。LibCYAML 库基于 LibYAML，基于模式（Schema）驱动的方式将 YAML 文档解析为 C 数据结构，用法较 LibYAML 更为简易。

在 Debian 或 Ubuntu 之类的系统中，可使用以下命令安装 LibCYAML 开发包：

```console
$ sudo apt install libcyaml-dev
```
# C 结构体 -> YAML

假设 C 程序中存在一个字符串常量：

```C
char *foo = "Foo";
```

如何将其保存为 YAML 文档？这是一个简单的问题，若不借助 LibCYAML，反而更容易解决：

```C
FILE *fp = fopen("foo.yaml", "r");
if (!fp) {
        fprintf(stderr, "Failed to open file!");
        exit(EXIT_FAILURE);
}
fprintf(fp, "foo: %s\n", foo);
fclose(fp);
```

但是，若借助 LibCYAML 解决该问题，则有助于初步理解 LibCYAML 库的工作过程，且该工作过程在使用 LibCYAML 解析 YAML 文档时同样需要。

上述字符串常量可保存为 YAML 的一个映射，不过必须将该字符串常量封装至一个结构体内，否则无法为 LibCYAML 提供 C 数据结构的 YAML 模式。以下是封装上述字符串常量的结构体：

```c
struct root {
        char *foo;
};
```

上述结构体只有一个域，对应的 YAML 模式可写为

```c
static const cyaml_schema_field_t root_mapping_schema[] = {
        CYAML_FIELD_STRING_PTR("foo", CYAML_FLAG_POINTER,
                               struct root, foo, 0, CYAML_UNLIMITED),
        CYAML_FIELD_END
};
```

上述代码可以令 LibCYAML 知悉 `struct root` 类型的 `foo` 成员是指针类型，指向一个长度无限制的字符串。

`struct root` 本身对应的 YAML 模式是映射，可写为

```c
static const cyaml_schema_value_t root_schema = {
        CYAML_VALUE_MAPPING(CYAML_FLAG_POINTER, struct root, root_mapping_schema)
};
```

对于 C 结构体：

```c
 struct root foo_wrapper = {.foo = "Foo"};
```


以下代码可将其转化为符合上述 YAML 模式的结果：

```c
static const cyaml_config_t config = {
        .log_fn = cyaml_log,            /* Use the default logging function. */
        .mem_fn = cyaml_mem,            /* Use the default memory allocator. */
        .log_level = CYAML_LOG_WARNING, /* Logging errors and warnings only. */
};
cyaml_err_t err = cyaml_save_file("foo.yaml", &config,
                                  &root_schema, &foo_wrapper, 0);
if (err != CYAML_OK) {
        exit(EXIT_FAILURE);
}
```

输出结果为 foo.yaml 文件，其内容应为

```yaml
foo: Foo
```

下面是完整的代码：

```c
#include <stdlib.h>
#include <stdio.h>
#include <cyaml/cyaml.h>

struct root {
        char *foo;
};

static const cyaml_schema_field_t root_mapping_schema[] = {
        CYAML_FIELD_STRING_PTR("foo", CYAML_FLAG_POINTER,
                               struct root, foo, 0, CYAML_UNLIMITED),
        CYAML_FIELD_END
};

static const cyaml_schema_value_t root_schema = {
        CYAML_VALUE_MAPPING(CYAML_FLAG_POINTER, struct root, root_mapping_schema)
};

int main(void)
{
        struct root foo_wrapper = {.foo = "Foo"};
        /* Create our CYAML configuration. */
        static const cyaml_config_t config = {
                .log_fn = cyaml_log,            /* Use the default logging function. */
                .mem_fn = cyaml_mem,            /* Use the default memory allocator. */
                .log_level = CYAML_LOG_WARNING, /* Logging errors and warnings only. */
        };
        cyaml_err_t err = cyaml_save_file("foo.yaml", &config,
                                          &root_schema, &foo_wrapper, 0);
        if (err != CYAML_OK) {
                exit(EXIT_FAILURE);
        }
        return EXIT_SUCCESS;
}
```

假设将上述内容保存为 foo.c 文件，以下命令可完成程序编译、运行以及结果查看：

```c
$ gcc foo.c -o foo -lcyaml
$ ./foo
$ cat foo.yaml
foo: Foo
```

由上述示例可见，LibCYAML 可将 C 结构体的成员（域）转化为 YAML 文档中的映射（键值对），但该结构体自身不会出现在 YAML 文档中，亦即从 YAML 视角看， C 结构体是包含一个或多个映射的容器，像 YAML 文档的起止符。

在为 C 数据结构编写 YAML 模式时，要注意区分域模式和值模式，前者类型为 `cyaml_schema_field_t`，用于构造结构体各成员的模式，后者类型为 `cyaml_schema_value_t`，用于构造结构体、数组以及基本类型的模式。

# C 数组 -> YAML


假设有一个 C 数组

```c
int a[] = {1, 2, 3, 4};
```

该如何将其转化为 YAML 的序列呢？

首先写出数组 `a` 的成员对应的 YAML 模式。`a` 的成员皆为整形数，因此相应的 YAML 值模式为

```c
static const cyaml_schema_value_t entry_schema = {
        CYAML_VALUE_INT(CYAML_FLAG_DEFAULT, int)
};
```

数组 `a` 本身对应的 YAML 值模式为

```C
static const cyaml_schema_value_t root_schema = {
        CYAML_VALUE_SEQUENCE(CYAML_FLAG_POINTER, int, &entry_schema, 0, CYAML_UNLIMITED)
};
```

以下代码实现了 C 数组向 YAML 序列的转换过程：

```c
int a[] = {1, 2, 3, 4};
static const cyaml_config_t config = {
        .log_fn = cyaml_log,
        .mem_fn = cyaml_mem,
        .log_level = CYAML_LOG_WARNING, 
};
cyaml_err_t err = cyaml_save_file("foo.yaml", &config, &root_schema, a, 4);
if (err != CYAML_OK) {
        exit(EXIT_FAILURE);
}
```

# YAML -> C 数据结构

LibCYAML 开发者已为 YAML 数据的解析提供了一篇简单的入门文档，我无需越俎代庖，详见「<https://github.com/tlsa/libcyaml/blob/main/docs/guide.md>」。

不过，有一个困扰我很久的问题，即如何解析无名的映射序列。LibCYAML 作者在该项目的 issue 列表中回答了这个问题，他的答案为

```c
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <cyaml/cyaml.h>

enum gender {
	MALE, FEMALE, UNKNOWN
};

struct person {
	char *name;
	unsigned age;
	enum gender gdr;
};

static const cyaml_strval_t gender_strings[] = {
	{"male", MALE},
	{"female", FEMALE},
	{"unknown", UNKNOWN},
};

static const cyaml_schema_field_t entry_fields_schema[] = {
	CYAML_FIELD_STRING_PTR("name", CYAML_FLAG_POINTER,
			struct person, name, 0, CYAML_UNLIMITED),
	CYAML_FIELD_UINT("age", CYAML_FLAG_DEFAULT,
			struct person, age),
	CYAML_FIELD_ENUM("gender", CYAML_FLAG_DEFAULT,
			struct person, gdr, gender_strings,
			CYAML_ARRAY_LEN(gender_strings)),
	CYAML_FIELD_END
};

static const cyaml_schema_value_t entry_schema = {
	CYAML_VALUE_MAPPING(CYAML_FLAG_DEFAULT,
			struct person, entry_fields_schema),
};

static const cyaml_schema_value_t people_schema = {
	CYAML_VALUE_SEQUENCE(CYAML_FLAG_POINTER, struct person,
			&entry_schema, 0, CYAML_UNLIMITED)
};

int main(void)
{
	cyaml_err_t err;
	struct person *people;
	unsigned people_count;
	static const char *yaml =
			"- name: \"Jason\"\n"
			"  age: 23\n"
			"  gender: male\n"
			"- name: \"Lisa\"\n"
			"  age: 26\n"
			"  gender: female\n"
			"- name: \"A\"\n"
			"  age: 20\n"
			"  gender: unknown\n";
	static const cyaml_config_t config = {
			.log_level = CYAML_LOG_NOTICE,
			.mem_fn = cyaml_mem,
			.log_fn = cyaml_log,
			.flags = CYAML_CFG_IGNORED_KEY_WARNING,
	};

	err = cyaml_load_data(
			(uint8_t *)yaml, strlen(yaml),
			&config, &people_schema,
			(void **)&people, &people_count);
	if (err != CYAML_OK) {
		fprintf(stderr, "ERROR: %s\n", cyaml_strerror(err));
		return EXIT_FAILURE;
	}

	printf("People count: %u\n", people_count);
	for (unsigned i = 0; i < people_count; i++) {
		printf("Person:\n");
		printf("\tname: %s\n", people[i].name);
		printf("\tage: %u\n", people[i].age);
		printf("\tgender: %i\n", people[i].gdr);

		free(people[i].name);
	}

	free(people);

	return EXIT_SUCCESS;
}
```

# 若想知道更多……

请参考 cyaml.h 中的注释。此外， Zrythm 项目也为 LibCYAML 的用法给出了一些介绍，详见「<https://docs.zrythm.org/cyaml_schemas.html>」。

