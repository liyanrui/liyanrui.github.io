---
title: Bash 的真谛
homeback: ../../index.html
date: 2023 年 04 月 16 日
...

虽然我已经用了 Bash 将近 20 年，几乎每天都在使用它，但我确定自己绝非 Bash 专家，充其量只是小学生级别，此事可从多年前我写过的一篇文章「写给高年级小学生的《Bash 指南》(https://segmentfault.com/a/1190000017229619)」获得证明。现在让我写一份 Bash 脚本，即使是完成非常简单的任务，我依然要从网络上搜索 Bash 的语法。现在 Chat GPT 大行其道了，问问它可能会更好一些。不过，Bash 的真谛可能恰恰是 Bash 专家们难以发现的，原因很简单，因为他们并非天才。

Bash 作为一门历史悠久的 Shell 语言，体内流淌的自然是 Unix 的血液。Unix 发明人之一，C 语言之父 Dennis Ritchie 曾经说过，Unix 是简单的，但只有天才能够理解这种简单。对于 Bash 也是如此。

Ritchie 所说的天才，并非世俗所认为的那种天才。Unix 的本质，亦即它体现的哲学，是反世俗的，如同老子的学说那样反世俗。Ritchie 所说的那句话，两千多年前的老子早已说过，「我言甚易知，甚易行，而世人莫能知，莫能行」。任何一个人，倘若他的思想和言行一贯地反世俗，基本上意味着他已抵仙境了。世俗里的一切，镜花水月，唯有反世俗，方能长存。Unix 自诞生之日起，各种预言信誓旦旦，说它必将衰败，然而现在 Unix 不仅没有衰败，反而繁衍出为 Linux，BSD，macOS 等众多变体。

许多人对 Bash 的批判，诸如

* Bash 语法的丑陋简直令人咋舌。这是一种典型的 quick and dirty，各种临时方案风格的设计，一致性很糟糕……
* 跟 Java 或者 Scheme 这样的语言截然不同，“脚本语言”往往意味着异常拙劣的设计，它的设计初衷往往是目光短浅的。这些语言里面充满了历史遗留下来的各种临时的 hack，几乎没有“原则”可言。Unix 的 shell（比如 bash，csh，……），一般都是这样的语言……
* 计算机科学家David A. Wheeler在 邮件列表上指出，bash的解析器存在许多漏洞，因为它在设计时就没有考虑过安全性，除非它停止解析环境变量，否则修正就像是打地鼠。
* 从理念上微软的 Powershell 比 Bash 先进一代，Powershell 有完整的、强类型的编程支持，Bash 里面全是字符串。Powershell 里面很多的函数比 UNIX 更加正交，如 % 和 ?，可以提供极其可怕的抽象能力，你 Bash 就是做不到……
* Unix shell 比 Powershell（以及其他带类型的 shell 比如还没做出来就魂归天国的 nosh）确实落后一代。单说一点，bash 之类里面的 * 是 shell 展开的，而 PS 是应用程序展开，后者显然要更合理（并不是每个参数都是路径）。

都是正确的。只是他们所批判的，恰恰是 Bash 所坚持的。俗人昭昭，我独昏昏。俗人察察，我独闷闷。Unix 是如此，Bash 自然也是如此，它们都是简单的。理解不了这种精神，自然就无法理解这种简单。

他们对 Bash 的批判，总体反映的是，Bash 不适合编写较大的程序，只适合写短小的脚本。事实上，Bash 甚至都不希望它的用户去写脚本。Eric Raymond 为了科普 Unix 的精神，仿照禅宗公案杜撰了一个无名师与万行码的段子，虽有故弄玄虚之嫌，但也是尽全力向大众宣传 Unix 的哲学。这个段子如下：


> Master Foo once said to a visiting programmer: “There is more Unix-nature in one line of shell script than there is in ten thousand lines of C.”
> 
> 无名师曾对一名来访的程序员说：「一行 Shell 脚本所体现的 Unix 本质要胜过万行 C 代码。」
> 
> The programmer, who was very proud of his mastery of C, said: “How can this be? C is the language in which the very kernel of Unix is implemented!”
> 
> 这位程序员，一向为以自己对 C 的精通为傲。他说：「怎么可能呢？就连 Unix 的内核都是用 C 语言写的！」
> 
> Master Foo replied: “That is so. Nevertheless, there is more Unix-nature in one line of shell script than there is in ten thousand lines of C.”
> 
> 无名师说：「你说的对，不过，一行 Shell 脚本所体现的 Unix 本质要胜过万行 C 代码。」
> 
> The programmer grew distressed. “But through the C language we experience the enlightenment of the Patriarch Ritchie! We become as one with the operating system and the machine, reaping matchless performance!”
> 
> 程序员烦躁起来，「但是在 C 语言里，我们经受尊者 Ritchie 的启蒙！人、操作系统、机器合为一体，获得无与伦比的效率！」
> 
> Master Foo replied: “All that you say is true. But there is still more Unix-nature in one line of shell script than there is in ten thousand lines of C.”
> 
> 无名师说：「你说的都是对的。但是，一行 Shell 脚本所体现的 Unix 本质要胜过万行 C 代码。」
> 
> The programmer scoffed at Master Foo and rose to depart. But Master Foo nodded to his student Nubi, who wrote a line of shell script on a nearby whiteboard, and said: “Master programmer, consider this pipeline. Implemented in pure C, would it not span ten thousand lines?”
> 
> 程序员呵呵了无名师，起身要离去。无名师点头示意了他的学生 Nubi。Nubi 在身边的白板上写了一行 Shell 脚本，然后说：「程序员大师，看看这个管道。单纯用 C，要不要用一万行？」
> 
> The programmer muttered through his beard, contemplating what Nubi had written. Finally he agreed that it was so.
> 
> 程序员捻须沉吟，竭力思考 Nubi 所写的 Shell 脚本。最终，他承认 Nubi 所说。
> 
> “And how many hours would you require to implement and debug that C program?” asked Nubi.
> 
> 「那么实现并调试那个 C 程序，你要花多少时间呢？」Nubi 问。
> 
> “Many,” admitted the visiting programmer. “But only a fool would spend the time to do that when so many more worthy tasks await him.”
> 
> 「很长」，这位来访的程序员承认。「不过，傻子才会花这么多时间来做此事，还有很多更值得做的事情等着他去做。」
> 
> “And who better understands the Unix-nature?” Master Foo asked. “Is it he who writes the ten thousand lines, or he who, perceiving the emptiness of the task, gains merit by not coding?”
> 
> 「那么，谁更理解 Unix 的本质？」无名师问。「是写一万行代码的人，还是看到任务的空无，不写代码却能获益的人？」
> 
> Upon hearing this, the programmer was enlightened.
> 
> 听了这些，程序员茅塞顿开。


Bash 语言的初衷并非方便使用者编程的语言，而是方便组合系统中现有的程序完成复杂的任务，故而上述对 Bash 的批判，犹如在批判一个男人没法怀孕，虽然正确，但皆为正确的废话。

对于任何一个任务，倘若能够通过 Bash 语言将一些现有的程序简单地组合起来便能得以妥善解决，从而避免使用某种语言为其编写程序，这是使用 Bash 的正确方法。如果 Bash 无法解决某个问题，你可以使用自己喜欢的任何一门语言为其编写一个程序，然后令其融入 Bash 环境，这是使用 Bash 的另一正确方法，而这正是前一种方法的基础。这就是 Bash 的真谛。
