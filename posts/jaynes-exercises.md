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

由于 $m$ 的分解可能不止一个，对于每个这样的集合，都有着上述的概率。并且这些集合不相互依赖，因此从瓮中所取 $m$ 个球能够覆盖所有颜色的概率为所有的 $\\{m_1,\cdots,m_k\\}$ 的概率之和，即

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k)
$$

展开可得

$$
\frac{\begin{pmatrix} N_1\\\\ m_{11}\end{pmatrix}\cdots\begin{pmatrix} N_k\\\\ m_{1k}\end{pmatrix} + \cdots + \begin{pmatrix} N_k\\\\ m_{r1}\end{pmatrix}\cdots\begin{pmatrix} N_k\\\\ m_{rk}\end{pmatrix}}{\begin{pmatrix} N\\\\ m\end{pmatrix}}
$$

其中 $m_{ij}$ 表示第 $i$ 组 $m$ 的分解的第 $j$ 个元素。由于 $m_{ij} \le 1$，所以上式分子部分可以展开为

$$
(N_1\cdots N_k)\left(\frac{1}{m_{11}\cdots m_{1k}}\begin{pmatrix} N_1 - 1\\\\ m_{11} - 1\end{pmatrix}\cdots\begin{pmatrix} N_k - 1\\\\ m_{1k} - 1\end{pmatrix} +  \cdots + \frac{1}{m_{r1}\cdots m_{rk}}\begin{pmatrix} N_1 - 1\\\\ m_{r1} - 1\end{pmatrix}\cdots\begin{pmatrix} N_k - 1\\\\ m_{rk} - 1\end{pmatrix}\right)
$$

由于 $m_{i1} + \cdots + m_{ik} = m$，可以确定当 $m_{i1} = \cdots = m_{ik}$ 时，$m_{i1}\cdots m_{ik}$ 为最大值，亦即上式有最小值。又由于$\\{m_{ij}\\}$ 的数量等价于将 $m$ 个数据分为 $k$ 组的可行解的个数。在《统计学习基础——数据挖掘、推理与预测》（Trevor Hastie 等（著），范明 等（译），p321）中指出，共有

$$
C = \frac{1}{k!}\sum_{i = 1}^k(-1)^{k - i}\begin{pmatrix}k\\\\ i\end{pmatrix}i^m
$$

个可行解。

因此，有

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k) \ge \frac{C\begin{pmatrix} N_1\\\\ \frac{m}{k}\end{pmatrix}\cdots\begin{pmatrix} N_2\\\\ \frac{m}{k}\end{pmatrix}}{\begin{pmatrix} N\\\\ m\end{pmatrix}}
$$


在 $k = 5$，所有的 $N_i = 10$ 的情况下，有

$$
\sum_{\\{m_i\\}}h(m_1\cdots m_k|N_1,\cdots,N_k) \ge 90\%
$$

因此

$$
\frac{\left(\frac{1}{5!}\sum_{i = 1}^5(-1)^{5 - i}\begin{pmatrix}5\\\\ i\end{pmatrix}i^m\right)\begin{pmatrix} 10\\\\ \frac{m}{5}\end{pmatrix}^5}{\begin{pmatrix} 50\\\\ m\end{pmatrix}} = 90\%
$$
