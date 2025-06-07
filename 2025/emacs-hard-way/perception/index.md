---
title: 感知
abstract: 知白守黑，为天下式。
date: 2025 年 06 月 01 日
...

# 前言

现在，我们尝试用 Elisp 编程来解一道应用题。这道应用题对我而言，颇为重要，对你而言，可作学习 Elisp 编程一例。假设 Emacs 的当前缓冲区内存在一些形如以下内容的片段：

```c
@ 这是一段 C 代码 #
int foo(void) {
    return 42;
}
@
```

同时，当前缓冲区内也有一些其他内容，但我们无需关心。现在，光标是落在上述片段内的，例如落在数字 `42` 的 `4` 上。我们看到的现象是如此，但是能否通过 Elisp 程序感知光标正处于这样的区域内呢？

为了让问题更明确一些，可将上述片段抽象为以下形式：

```
@ 片段名称 #
片段内容
@
```

上述形式中，片段名称不会包含 `#` 字符，片段内容中也不存在任何一行文字只含有字符 `@` 的情况。于是，我们的问题便可以明确为，当光标处于片段内容区域，此时能否通过 Elisp 感知光标处于上述形式的片段内呢？为了便于描述，我们将上述抽象的片段形式称为 [Orez 形式](../../orez-v1/index.html)。

# 算法

我们可以从光标当前位置出发，向后（向缓冲区首部方向）遍历缓冲区，并探测何时遇到以 `@` 开头且以 `#` 结尾的一行文字，并且也向前（向缓冲区尾部方向）遍历缓冲区，并探测何时遇到只包含 `@` 的一行文字，若这两个方向的探测皆有所得，便可判定光标正处于 Orez 形式区域。

上述算法并不困难，关键在于，如何判断一行文字是否含有 `@` 开头且以 `#` 结尾，以及是否只包含 `@`。这两个关键问题，我们可基于 Emacs 提供的字符串匹配函数予以解决。

# 正则表达式

正则表达式，是一种微型语言，可用于描述文字模式。例如，一段文字，我们知道它是以 `@` 开头且以 `#` 结尾，且除首尾外，其他文字皆非 `#`，对于这种形式的文字，用正则表达式可表述为 `^@[^#]+#$`。倘若你从未了解过正则表达式，应该会觉得这是蕴含某种神秘力量的咒语。事实上，只要略加解释，你便会明白一切都很简单。

* `^` 表示一段文字的首部。
* `[^#]` 表示一个字符，它不是 `#`。
* `[^#]+` 表示存在一个或多个非 `#` 字符。
* `#` 就是字符 `#`。
* `$` 表示一段文字的尾部。

我们也可以让上述正则表达式所表达的文字模式更为宽泛一些，例如 `^[ \t]*@[^#]+#[ \t]*$`，其中 `[ \t]` 表示一个字符，它可以是空格，也可以是制表符（即使用 Tab 键输入的字符），而 `[ \t]*` 则表示存在 0 个或 1 个或更多个字符，它们或为空格，或为制表符。

可以用 Emacs 提供的 `string-match` 进行一些试验。例如

```lisp
(let ((x "@ i am foo #"))
  (if (string-match "^@[^#]+#$" x)
      (message "hit!")
    (message "failed!"))) ;; 会输出 hit!
```

再例如

```lisp
(let ((x "    @ i am foo #"))
  (if (string-match "^@[^#]+#$" x)
      (message "hit!")
    (message "failed!"))) ;; 会输出 failed!
```

再例如

```lisp
(let ((x "    @ i am foo #"))
  (if (string-match "^[ \t]*@[^#]+#$" x)
      (message "hit!")
    (message "failed!"))) ;; 会输出 hit!
```

凡是能让 `string-match` 的求值结果为真，即为 `t` 的正则表达式和字符串，称二者匹配。Emacs 所支持的正则表达式，有一个功能是允许我们从它所匹配的字符串中捕获一些文字。例如，捕获上述最后一个示例中 `x` 的 `i am foo` 部分，只需将与之匹配的正则表达式修改为

```
^[ \t]*@[ \t]*\\([^#]+\\)[ \t]*#$
```

其中 `\\(` 和 `\\)` 表示可捕获它们所包围的部分，即 `[^#]+`。捕获结果可通过 `match-string` 获取，例如

```lisp
(let ((x "    @ i am foo #"))
  (if (string-match "^[ \t]*@\\([^#]+\\)#$" x)
      (message "%s" (match-string 1 x))
    (message "failed!"))) ;; 会输出 i am foo
```

`match-string` 的第 1 个参数表示获取第几个捕获，由于上述代码中只有一处捕获，故该参数为 1。

也许你已经感受到了正则表达式的强大，它能对字符串实现模糊匹配，可是你应该也能感受到它的弊端，一旦要匹配的文本较为复杂，为其所写的正则表达式很快你便难解其意了，亦即复杂的正则表达式几乎不具备可维护性。

# rx 记法

为了让正则表达式具备可维护性，Emacs 提供了 rx 记法，亦即你可以通过 rx 表达式构造正则表达式。例如

```lisp
(rx line-start (zero-or-more (any " \t"))
    "@"
    (one-or-more (not "#"))
    "#"
    (zero-or-more (any " \t")) line-end)
```

其求值结果为

```
"^[ \t]*@[^#]+#[ \t]*$"
```

也可以用 `rx-let` 表达式，定义一些局部变量，将其作为一些正则表达式的「简写」，例如以下代码与上文的 `rx` 表达式等效。

```lisp
(rx-let ((padding (zero-or-more (any " \t")))
         (name-area (one-or-more (not "#"))))
  (rx line-start padding "@" name-area "#" padding line-end))
```

注意，在 rx 记法中，使用局部变量作为正则表达式记号，只能用 `rx-let`，而不能用 `let`。

若需要构造带有捕获的正则表达式，在 rx 记法可使用 `group`。例如

```lisp
(rx-let ((padding (zero-or-more (any " \t")))
         (name-area (one-or-more (not "#"))))
  (rx line-start padding "@" (group name-area) "#" padding line-end))
```

求值结果为

```regex
"^[ \t]*@\\([^#]+\\)#[ \t]*$"
```

虽然正则表达式要比 rx 记法更简约，但是 rx 记法更容易让我们理解正则表达式的结构，故而以后我们尽量在 Elisp 中使用 rx 记法，而非正则表达式。以下是 rx 记法的应用示例：

```lisp
(let ((x "    @ i am foo #")
      (re (rx-let ((padding (zero-or-more (any " \t")))
                   (name-area (one-or-more (not "#"))))
            (rx line-start padding "@" (group name-area) "#" line-end))))
  (if (string-match re x)
      (message "%s" (match-string 1 x))
    (message "failed!"))) ;; 会输出 i am foo
```

# 感知

希望你还没有忘记我们的使命，从当前缓冲区的光标所在位置向后探测，寻找正则表达式 `^@[^#]+#$` 可匹配的一行文字，此事现在已无任何难点。

```lisp
(defun orez-search-backward ()
  (let (re line)
    (setq re (rx line-start "@"
                 (one-or-more (not "#"))
                 "#" line-end))
    (catch 'break
      (while t
        (setq line (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (if (string-match re line)
            (throw 'break (point))
          (progn
            (when (<= (point) (point-min))
              (throw 'break nil))
            (forward-line -1))))
      nil)))
```

为了便于你理解上述代码，我将其翻译成了以下 C 语言伪代码：

```c
int orez_search_backward(void) {
    Regex re = 由 rx 记法构造的正则表达式;
    while (1) {
        String line = 当前的一行文字;
        if (re 与 line 匹配) {
            return point();
        } else {
            if (point() <= point_min()) {
                return -1; /* 返回无效位置，表示探测失败 */
            }
            forward_line(-1); /* 后退一行 */
        }
    }
    return -1; /* 返回无效位置，表示探测失败 */
}
```

向前探测过程，要比向后探测略微简单一些，下面我直接定义它，且不再以 C 伪代码予以注释。

```lisp
(defun orez-search-forward ()
  (let (re line)
    (setq re (rx line-start "@" line-end))
    (catch 'break
      (while t
        (setq line (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (if (string-match re line)
            (throw 'break (point))
          (progn
            (when (>= (point) (point-max))
              (throw 'break nil))
            (forward-line))))
      nil)))
```

基于 `orez-search-backward` 和 `orez-search-forward` 的结果便可确定光标是否落在 Orez 形式区域。

```lisp
(defun in-orez-area? ()
  (if (and (orez-search-backward) (orez-search-forward))
      t
    nil))
```

上述代码使用了布尔运算中的「与」运算 `and`。Elisp 的布尔运算还有「或」运算 `or` 以及前文在构造 rx 记法时用过的「非」运算 `not`。基于这三种运算，可以构造复杂的逻辑表达式。

# bobp 和 eobp

`orez-search-backward` 和 `orez-search-forward` 的定义中，皆在 `while` 表达式中判断光标是否已抵达缓冲区首部和尾部，即

```lisp
(<= (point) (point-min))
```

和

```
(>= (point) (point-max))
```

实际上，Emacs 为上述这两种情况的判断提供了函数 `bobp` 和 `eobp`，故而可用 `(bobp)` 和 `(eobp)` 分别代替上述表达式。故而将 `orez-search-backward` 和 `orez-search-forward` 重新定义为

```lisp
(defun orez-search-backward ()
  (let (re line)
    (setq re (rx line-start "@"
                 (one-or-more (not "#"))
                 "#" line-end))
    (catch 'break
      (while (not (bobp))
        (setq line (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (if (string-match re line)
            (throw 'break (point))
          (forward-line -1)))
      nil)))
```

```lisp
(defun orez-search-forward ()
  (let (re line)
    (setq re (rx line-start "@" line-end))
    (catch 'break
      (while (not (eobp))
        (setq line (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (if (string-match re line)
            (throw 'break (point))
          (forward-line)))
      nil)))
```

# 现场保存

若光标在 Orez 形式区域，而你也真的试着用过 `in-orez-area?` 函数，便会发现，Emacs 对该函数求值后，光标会被移动到 Orez 形式区域的末尾。原因是 `orez-search-backward` 和 `orez-search-forward` 函数使用了逐行移动光标函数 `forward-line`。若想在应用 `in-orez-area?` 之后能将光标复原，你可以先用一个局部变量保存光标位置，时候再将光标移至该位置，例如

```lisp
(defun in-orez-area? ()
  (let ((x (point)))
    (if (and (orez-search-backward) (orez-search-forward))
        (progn
          (goto-char x)
          t)
      (progn
        (goto-char x)
        nil))))
```

Emacs 为了不让你如此费心，它提供了 `save-excursion` 表达式，可完成等效工作，其用法如下

```lisp
(defun in-orez-area? ()
  (save-excursion
    (if (and (orez-search-backward) (orez-search-forward))
        t
      nil)))
```

**练习**：若 Orez 形式更为复杂，例如片段名称可能跨越多行，行间以 `\` 连接，例如

```
@ 这是可跨越 \
  多行的片段名称 #
片段内容
@
```

此时，你该如何实现 `orez-search-backward` 函数呢？

# 总结

Orez 是我编写的文学编程工具。所谓文学编程，即程序的文档与代码是混合态，即文档片段和代码片段彼此纠缠。Orez 可从文学程序里抽取可编译/解释的完整代码，也可将文学程序转化为用于文档排版的源文件，由 TeX 或类似的排版软件生成程序文档。

我之所以需要在 Emacs 里识别 Orez 形式区域，是因为文学程序里可能存在多种编程语言的代码片段，Emacs 很难以统一的模式编辑它们。倘若能识别 Orez 区域，将这些代码片段临时提取到另一个窗口中的缓冲区，并开启相应的编程语言模式，则 Emacs 便可作为文学编程所用的专业编辑器了。

现在完成这一目的所需的 Elisp 语法和 Emacs 函数，我已经基本掌握了。你虽然没有我的追求，而你已经具备了驾驭 Emacs 的能力了。我们相忘于江湖的时间快要到了。
