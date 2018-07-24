<!--
.. title: Jaynes 概率论习题答案
.. slug: jaynes-exercises
.. date: 2018-07-19 16:18:11 UTC+08:00
.. tags: 
.. category: 概率论
.. link: 
.. description: 
.. type: text
-->

# 习题 3.2

假设瓮内含有 $N = \sum N_i$ 个球，$N_1$ 个颜色 $1$ 的，$N_2$ 个颜色 $2$ 的，$\cdots$，$N_k$ 个颜色 $k$ 的。从瓮中无放回地取出 $m$ 个球；每种颜色的球至少取一个的概率是多大？假设 $k = 5$，所有的 $N_i = 10$，若得到颜色全集的概率为 90\%，需要取多少次？

假设由 $k$ 个正数构成的集合 $\\{m_1,\cdots,m_k\\}$，当它满足 $m_1 + \cdots + m_k = m$ 时，将其称为 $m$ 的分解。$m_i$ 表示第 $i$ 种颜色的球的个数。根据广义超几何分布公式，可得

$$
h(m_1\cdots m_k|N_1,\cdots,N_k) = \frac{\begin{pmatrix} N_1\\\\ m_1\end{pmatrix}\cdots\begin{pmatrix} N_k\\\\ m_k\end{pmatrix}}{\begin{pmatrix} N\\\\ m\end{pmatrix}}
$$

由于 $m$ 的分解可能不止一个，并且各个分解不相互依赖，因此从瓮中所取 $m$ 个球能够覆盖所有颜色的概率为 $m$ 所有的分解对应的概率之和。

先看一下较为简单的情况。假设 $k = 2$，$N_1 = 3$，$N_2 = 4$，$m = 3$，则得到颜色全集的概率为

$$
\frac{\begin{pmatrix}3 \\\\ 1\end{pmatrix}\begin{pmatrix}4 \\\\ 2\end{pmatrix} + \begin{pmatrix}3 \\\\ 2\end{pmatrix}\begin{pmatrix}4 \\\\ 1\end{pmatrix}}{\begin{pmatrix}7\\\\ 3\end{pmatrix}} = \frac{6}{7}
$$


假设 $k = 3$，$N_1 = 3$，$N_2 = 4$，$N_3 = 2$，$m = 4$，则得到颜色全集的概率为

$$
\frac{\begin{pmatrix}3 \\\\ 1\end{pmatrix}\begin{pmatrix}4 \\\\ 1\end{pmatrix}\begin{pmatrix}2 \\\\ 2\end{pmatrix} + \begin{pmatrix}3 \\\\ 1\end{pmatrix}\begin{pmatrix}4 \\\\ 2\end{pmatrix}\begin{pmatrix}2 \\\\ 1\end{pmatrix} + \begin{pmatrix}3 \\\\ 2\end{pmatrix}\begin{pmatrix}4 \\\\ 1\end{pmatrix}\begin{pmatrix}2 \\\\ 1\end{pmatrix}}{\begin{pmatrix}9\\\\ 4\end{pmatrix}} = \frac{4}{7}
$$
