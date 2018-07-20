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

假设有 $k$ 个正数 $\\{m_1,\cdots,m_k\\}$，$m_1 + \cdots + m_k = m$，根据广义超几何分布公式，可得

$$
h(m_1\cdots m_k|N_1,\cdots,N_k) = \frac{\begin{pmatrix} N_1\\\\ m_1\end{pmatrix}\cdots\begin{pmatrix} N_k\\\\ m_k\end{pmatrix}}{\begin{pmatrix} N\\\\ m\end{pmatrix}}
$$

由于满足 $m_1 + \cdots + m_k = m$ 的 $\\{m_1,\cdots,m_k\\}$ 可能不止一个，对于每个这样的集合，都有着上述的概率。并且这些集合不相互依赖，因此从瓮中所取 $m$ 个球能够覆盖所有颜色的概率为所有的 $\\{m_1,\cdots,m_k\\}$ 的概率之和，即

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k)
$$

在 $k = 5$，所有的 $N_i = 10$ 的情况下，有

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k) \ge 90%
$$

展开可得

$$
\ \ge 90%
$$
