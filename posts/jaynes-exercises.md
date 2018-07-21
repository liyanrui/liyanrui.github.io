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

假设由 $k$ 个正数构成的集合 $\\{m_1,\cdots,m_k\\}$，当它满足 $m_1 + \cdots + m_k = m$ 时，将其称为 $m$ 的分解。$m_i$ 表示第 $i$ 种颜色的球的个数。根据广义超几何分布公式，可得

$$
h(m_1\cdots m_k|N_1,\cdots,N_k) = \frac{\begin{pmatrix} N_1\\\\ m_1\end{pmatrix}\cdots\begin{pmatrix} N_k\\\\ m_k\end{pmatrix}}{\begin{pmatrix} N\\\\ m\end{pmatrix}}
$$

由于 $m$ 的分解可能不止一个，并且各个分解不相互依赖，因此从瓮中所取 $m$ 个球能够覆盖所有颜色的概率为 $m$ 所有的分解对应的概率之和，即

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k)
$$

根据《统计学习基础——数据挖掘、推理与预测》（Trevor Hastie 等（著），范明 等（译），p321）所述，$m$ 的不同的分解，个数为

$$
C = \frac{1}{k!}\sum_{i = 1}^k(-1)^{k - i}\begin{pmatrix}k\\\\ i\end{pmatrix}i^m
$$

