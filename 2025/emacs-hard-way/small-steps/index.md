---
title: 跬步
abstract: 山高万仞，只登一步。
date: 2025 年 05 月 31 日
...

# 前言

现在，Emacs 的缓冲区，你应该已经能够招之即来，挥之即去了，应该也能在众多缓冲区之间随意穿越，也应该随时能让某个缓冲区呈现在某个窗口里纤毫毕露。不过，若要充分运用缓冲区蕴含的力量，你还需要学会一些简单的 Elisp 表达式，通过它们可以触摸缓冲区内的一切。

# 递归

你已经知道了，通过 `point-min` 和 `point-max` 函数可以获得当前缓冲区的首部和尾部位置，也已经知道了可以通过 `forward-char` 和 `backward-char` 函数可以在缓冲区内移动光标，但是你知道如何从当前缓冲区的起始位置开始，依序触摸每个字符，直至缓冲区尾部吗？

假设当前缓冲区一共有 5 个字符，例如「Hello」，我可以写出像下面这样的愚蠢的函数，它刚好能遍历 5 个字符。

```lisp
(defun stupid-walker ()
  (message "%c" (char-after 1))
  (message "%c" (char-after 2))
  (message "%c" (char-after 3))
  (message "%c" (char-after 4))
  (message "%c" (char-after 5)))
```

Emacs 函数 `char-after` 可以获得位于光标之后的第一个字符。要获得光标之前的第一个字符，可以用 `char-before`。`message` 表达式中的 `%c` 用于格式化输出字符。在上述假想的场景里，若对表达式 `(stupid-walker)` 求值，`*Messages*` 缓冲区则有以下输出：

```
H
e
l [2 times]
o
```

我们可以让 `stupid-walker` 变得聪明一些，例如

```lisp
(defun clever-walker (i)
  (message "%c" (char-after i)))
```

若要依序遍历上述假想的缓冲区中每个字符，只需

```lisp
(clever-walker 1)
(clever-walker 2)
(clever-walker 3)
(clever-walker 4)
(clever-walker 5)
```

**直到有一天，这个聪明的行者忽然开悟了，圣人之道，吾性自足，不假外求！**我何必一直等待外界修改 `i` 的值再传给我呢，我完全可以自己修改，而且我甚至可以自己对自己求值：

```lisp
(defun clever-walker (i)
  (message "%c" (char-after i))
  (clever-walker (+ i 1)))
```

之后，`clever-walker` 发现，它只需要从外界获得 1，然后就能一直走到地老天荒了，天得一以清，地得一以宁，侯王得一为天下正。

```lisp
(clever-walker 1)
```

当 `clever-walker` 发现自己的确可以从 1 开始，一直走下去，并触碰到了当前缓冲区里的每个字符，然后他只需明白在何处停止前进，便完成了任务。

```lisp
(defun clever-walker (i)
  (when (< i (point-max))
    (message "%c" (char-after i))
    (clever-walker (+ i 1))))

(clever-walker 1)
```

上述代码中的 `(< i (point-max))`，想必你能才出它的含义，即比较 `i` 是否小于 `(point-max)`。`clever-walker` 是我们用 Elisp 写的第一个递归函数。

一个函数，若它对自身求值，便是递归的。也许 Elisp 的作者担心我们没有王阳明龙场悟道的天赋，故而创造了 `while` 表达式，使得不开悟的人也能做到开悟者能做到的事情，例如

```lisp
(defun clever-walker (i)
    (while (< i (point-max))
      (message "%c" (char-after i))
      (setq i (+ i 1))))

(clever-walker 1)
```

上述代码中，Emacs 可对 `while` 之后第一个条件表达式 `(< i (point-max))` 求值，若结果为真，即 `t`，则对后续表达式依序求值，然后周而复始，直至第一个条件表达式的求值结果为假，即 `nil`。也许你依然记得，Elisp 里的任何表达式，必须要有求值结果。`while` 表达式的求值结果总是 `nil`，亦即它不需要结果。

# 再谈 let

倘若采用移动光标的方式遍历缓冲区，上述 `clever-walker` 的参数 `i` 便可省去。例如

```lisp
(defun clever-walker ()
	(goto-char (point-min)) ;; 将光标移到当前缓冲区首部
    (while (< (point) (point-max))
      (message "%c" (char-after (point)))
      (forward-char)))

(clever-walker)
```

上述代码中的 `point` 函数可以获取光标当前位置。原先由参数 `i` 完成的工作，现在由 `point` 函数完成，程序的性能会有所降低，不过也不必为此而焦虑，两种方式皆可用。若在意性能，应该将 `clever-walker` 定义为

```lisp
(defun clever-walker ()
  (goto-char (point-min))
  (let (i n)
    (setq i (point))
    (setq n (point-max))
    (while (< i n)
      (message "%c" (char-after i))
      (forward-char)
      (setq i (point)))))
```

不知你是否注意到了，在 `let` 表达式定义局部变量时，可以不对其赋予初值，直接像函数参数那样，构造出局部变量列表，然后在后续的过程中用 `setq` 赋值。未赋初值的局部变量，其值默认是 `nil`，亦即

```lisp
(let (a b c)
  ... ... ...)
```

等效于


```lisp
(let ((a nil)
      (b nil)
	  (c nil))
  ... ... ...)
```

亦等效于

```lisp
(let (a b c)
  (setq a nil
        b nil
		c nil)
  ... ... ...)
```

# 逐行遍历

Emacs 函数 `forward-line` 可将光标移动到下一行的开头。基于该函数，我们能更大跨度遍历当前缓冲区。例如

```lisp
(defun clever-walker ()
  (goto-char (point-min))
  (while (< (point) (point-max))
    (message "%s" (buffer-substring (pos-bol) (pos-eol)))
    (forward-line)))

(clever-walker)
```

`buffer-substring` 可以从当前缓冲区提取指定区间的内容。`pos-bol` 和 `pos-eol` 分别用于获取光标所在的一行文本的首部和尾部位置，这两个位置便可构成 `buffer-substring` 所需的区间。

`pos-bol` 和 `pos-eol` 这两个函数是在 Emacs 29 版本引入的，之前它们的名字较长，分别是 `line-beginning-position` 和 `line-end-position`。

Emacs 没有提供 `backward-line` 函数，但是可以用 `(forward-line -1)` 将光标移动到上一行的首部。

由于 Emacs 功能丰富且强大，缓冲区内的文字有时会带有一些属性，例如字体和颜色等信息。`buffer-substring` 提取的内容，也会带有这些属性。倘若只是想提取纯粹的文字，应该用名字更长的 `buffer-substring-no-properties` 函数，其用法与 `buffer-substring` 同。

# 收集

有时，可能需要从缓冲区里汲取一部分内容。这一需求，可以在对缓冲区逐字或逐行遍历的过程中，以字符串累加的方式实现。Emacs 的 `concat` 函数，可将两个字符串连接起来，基于该函数可实现字符串累加。例如

```lisp
;; 求值结果应该为 hello world!
(let ((acc ""))
  (setq acc (concat acc "hello"))
  (setq acc (concat acc " world!"))
  (message "%s" acc))
```

基于上述代码，便可在遍历当前缓冲区的过程中收集文字，例如

```lisp
(defun walker ()
  (goto-char (point-min))
  (let ((acc ""))
    (while (< (point) (point-max))
      (setq acc (concat acc (string (char-after (point)))))
      (forward-char))
    acc))
```

`string` 函数可将一个或一系列的字符串联起来变为字符串。注意，上述代码最终以 `acc` 作为 `let` 表达式的求值结果。由于 `let` 表达式是函数 `walker` 定义中的最后一个表达式，故而其求值结果也是 `walker` 的求值结果，即当前缓冲区内的所有字符。倘若缓冲区内所有字符皆无属性，我们所写的 `walker` 函数，等效于 Emacs 提供的 `buffer-string` 函数，看似我们在白费工夫，实则不然，因为我们可以根据具体情况，随时终止文字收集过程，例如，遇「水」则止……

基于 `while` 表达式写出能遇「水」则停止的 `walker`，目前尚无法做到，原因是我们不知如何提前跳出 `while` 表达式所表达的周而复始的逻辑。用递归函数，也无法实现，因为需要处理的逻辑超出了 `when` 的表达能力，它只能表达对条件为真或为假的作出单一响应，而无法分别作出响应。此刻，我们触及了所掌握的知识的边界。

# 适可而止

Elisp 可通过 `catch` 和 `throw` 表达式实现一段程序的退出。`cache` 表达式用于在一段程序运行前设定退出标记。例如

```lisp
(catch 'here
  (message "step 1")
  (message "step 2")
  (message "step 3"))
```

对上述表达式求值，三个 `message` 表达式会被陆续求值，最后的 `message` 表达式的求值结果——字符串 `"step 3"` 会被 Emacs 作为 `catch` 表达式的求值结果。现在想必你已经对 Emacs 对 Elisp 程序的这种求值逻辑颇为熟悉了，即 Emacs 对一组表达式依序求值时，最后一个表达式的求值结果便是这组表达式的求值结果。另外，即使我之前没有说过，何谓 `message` 的求值结果，想必你也能猜到，`message` 会将自己输出的字符串作为求值结果。

倘若只是用于对一组表达式求值，那么 `catch` 并不比 `let` 更高明，关键在于 `catch` 在这组表达式之前设定的标记，后续的表达式可根据情况随时跳转至该标记所在位置，并在此提供一个求值结果作为 `catch` 表达式的求值结果，亦即在此终结 `catch` 表达式，该过程可通过 `throw` 表达式实现。例如

```lisp
(catch 'here
  (message "step 1")
  (throw 'here (message "step 2"))
  (message "step 3"))
```

上述的 `catch` 表达式，第 3 条 `message` 表达式永无机会被求值，因为在其之前，`throw` 表达式以第二条 `message` 表达式终结了 `catch` 表达式，亦即第二条 `message` 表达式的求值结果便是 `catch` 表达式的求值结果，故而对上述表达式求值，结果应该是 `"step 2"`。

基于 `catch/throw`，便可在遍历当前缓冲区的过程中实现遇「水」则止。例如

```lisp
(defun walker ()
  (goto-char (point-min))
  (let ((acc "") x)
    (catch 'break
      (while (< (point) (point-max))
	(setq x (char-after (point)))
	(when (char-equal x ?水)
	  (throw 'break acc))
	(setq acc (concat acc (string x)))
	(forward-char))
	acc)))
```

上述代码中，`(throw 'break acc)` 会以 `acc` 作为求值结果终结 `cache`。由于 `cache` 表达式是其外围的 `let` 表达式中最后一个表达式，其求值结果 `acc` 自然也是 `let` 表达式的求值结果。又由于 `let` 表达式是函数 `walker` 中的最后一个表达式，其求值结果自然也是 `walker` 的求值结果，故而 `walker` 在遇到「水」字时，求值结果便是当时的 `acc`。需要注意的是，倘若在遍历缓冲区的过程中一值未遇到「水」，`catch` 表达式最后的求值结果也是 `acc`，此时的 `acc` 保存的是当前缓冲区的全部内容。

需要注意的是，上述代码中使用了此前未曾用过的函数 `char-equal`，它可用于比较两个字符是否相同。`?水` 表示「水」字的字面量。在其他编程语言中，表达字符的字面量，通常用单引号，例如 `'a'` 表示字符 `a` 的字面量，但是这些编程语言却无法对汉字如此表示，原因是单引号表达的字面量，长度通常只有 1 个字节，而汉字都是多个字节的。相比之下，Elisp 的问号表示法可用于表达汉字字符，颇为先进的，你也可以用这种形式表达单字节字符，例如所有字母、数字、下划线等字符。

我知道，上述的 `walker` 函数的逻辑已颇为复杂了，理解起来并非易事。阅读 Elisp 代码或者其他 Lisp 语言的代码时，头脑需要保持清醒，记住程序的最后一个表达式的求值结果便是程序的求值结果，而每一条表达式都可以视为一段程序……还记得 Lisp 机的灵魂吗？的确存在很多 Lisp 语言，Elisp 只是其中之一。在计算机科学领域，一种被广为推崇的 Lisp 语言叫作 Scheme。

倘若你懂得一些 C 语言，我可以用伪 C 语言重新描述 `walker` 的逻辑，如下：

```c
String walker(void) {
        goto_char(point-min());
        String acc = "";
        while (point() < point_max()) {
                Char x = char_after(point());
                if (x 为 '水') return acc;
                acc = concat(acc, string(x));
                forward_char();
        }
        return acc;
}
```

上述代码只是伪 C 代码，只能用于表意，并不合乎 C 语法，但是通过它，也许你能彻底明白 Elisp 版本的 `walker` 函数所表达的逻辑。

# 条件表达式

Elisp 的条件表达式不止有 `when`，它也提供了很多其他编程语言都有的 `if` 表达式，只是形式上有些怪异，例如

```lisp
(let ((x ?火))
  (if (char-equal x ?水)
      (message "true")
    (message "false")))
```

上述表达式的求值结果是 `"false"`，且在 `*Messages*` 缓冲区输出 `false`。

`if` 表达的形式可表述为

```lisp
(if 逻辑表达式
    (程序分支 1)
  (程序分支 2))
```

若用伪 C 代码表达与上述逻辑等效的形式，即

```c
if (逻辑表达式
  程序分支 1
} else {
  程序分支 2
}
```

亦即 Elisp 的 `if` 表达式里是暗含 `else` 分支的，但是 `if` 表达式中的两个程序分支表达式，都是单一表达式，亦即无法写为一组表达式，例如

```lisp
(let ((x ?火))
  (if (char-equal x ?水)
      (message "true")
      (message "x 是水")
    (message "false")
    (message "水火不容")))
```

虽然上述表达式也能求值，但是并非我们所期望的，亦即程序分支 1 并非

```lisp
(message "true")
(message "x 是水")
```

而程序分支 2，也并非

```lisp
(message "false")
(message "水火不容")
```

我们可以再用一次伪 C 代码表达上述的 `if` 表达式的逻辑，如下

```c
Char x = '火';
if (x 为 '水') {
        message("true");
} else {
        message("x 是水");
}
message("false");
message("水火不容");
```

那么，我们该如何用 Elisp 的 `if` 表达我们想要的逻辑的，亦即表达与以下伪 C 代码相同的逻辑。

```c
Char x = '火';
if (x 为 '水') {
        message("true");
        message("x 是水");
} else {
        message("false");
        message("水火不容");  
}
```

答案是使用 `progn` 表达式，该表达式的功用类似上述伪 C 代码的花括号，可以将一组表达式包裹起来，成为一个表达式。例如

```lisp
(let ((x ?火))
  (if (char-equal x ?水)
      (progn
        (message "true")
        (message "x 是水"))
    (progn
      (message "false")
      (message "水火不容"))))
```

之前是因 Elisp 的 `if` 表达式有一些令人费解，所以我一直在用 `when`，但 `if` 终究是无法避开的。不过，实际上也不难，只要你掌握了 `if` 的逻辑以及 `progn` 的作用。

不过，`if` 只能对逻辑表达式成立和不成立两种情况作出响应。在很多情况下，我们需要响应条件是超越逻辑表达式的。其他编程语言提供了 `else if` 或 `elif` 作为附加的程序分支。例如，在 C 语言里可以实现以下逻辑：

```c
if (x 为 '水') {
        ... ... ...;
} else if (x 为 '火') {
        ... ... ...;
} else if (x 为 '木') {
        ... ... ...;
} else {
        ... ... ...;
}
```

对于上述逻辑，Elisp 的 `if` 无能为力，不过，Elisp 有 `cond` 表达式可模拟上述逻辑，例如

```lisp
(cond
  ((char-equal x ?水) (... ... ...))
  ((char-equal x ?火) (... ... ...))
  ((char-equal x ?木) (... ... ...))
  (t (... ... ...)))
```

请注意上述伪 Elisp 代码中的最后一行，我们用真值 `t` 模拟上述 C 伪代码里的 `else` 实现兜底处理，用于处理之前的条件皆不成立的情况。

现在，可以通过定义编写遇「水」则止的递归版本的 `walker` 函数让自己更加明白，为何 `if` 以及 `cond` 这样的条件表达式不可或缺。

```lisp
(defun walker (acc)
  (if (< (point) (point-max))
      (progn
        (let ((x (char-after (point))))
          (if (char-equal x ?水)
              acc
            (progn
              (setq acc (concat acc (string x)))
              (forward-char)
              (walker acc)))))
    acc))

;;walker 的用法
(progn
  (goto-char (point-min))
  (message "%s" (walker "")))
```

如同没有 `catch/throw` 的帮助，遇「水」则止的 `while` 表达式写不出来，若没有 `if` 的帮助，遇「水」则止的递归函数，也写不出来。也许你又一次看不懂 Elisp 代码了，不过你总是可以尝试将其翻译成伪 C 代码去明白它。例如

```c
void walker(String acc) {
        if (point() < point_max()) {
                Char x = char_after(point());
                if (x 为 '水') return acc;
                else {
                        acc = concat(acc, string(x));
                        forward_char();
                        walker(acc);
                }
        } else return acc;
}
```

# 总结

也许你会觉得本文有些费脑，实际上我也有同感，我国古代哲学家王阳明应该也有同感，特别当他发现圣人之道即为递归的那个瞬间。主要原因是，我们不经意间触及到了计算机编程的一些本质问题，即一些重要的程序逻辑可以基于有条件约束的递归予以实现，也可基于有条件中断的 `while` 循环予以实现，这两种方式是等效的，前者属于函数式编程，后者属于过程式编程。至于二者孰优孰劣，至今仍争论未休，Elisp 的看法是，你可以根据自己的偏好选择，亦可两者混用。你看，Emacs 用着用着，人自然就会一些编程了。
