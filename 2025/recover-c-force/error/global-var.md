全局变量被诟病的原因，上文略有提及，即在多线程环境中存在不安全风险。不过，C11 标准为线程局部存储（TLS）提供了 `_Thread_local` 关键字，用它修饰 `sim_err`，可保证在每个线程中都有一个 `sim_err` 的复本。使用这个办法，可以保证 `sim_err` 在多线程环境中的安全性。

以下代码针对支持 C11 标准的 C 编译器，实现 `sim_err` 的多线程安全性，而对于不支持 C11 标准的编译器，依然 `sim_err` 依然是线程不安全的全局变量，并驱使编译器给出一条警告。

```c
#if __STDC_VERSION__ >= 201112L
        _Thread_local SimErr sim_err;
#else
        SimErr sim_err;
        #warning "sim_err is not safe for multi-threaded access."
#endif
```

不理解上述代码也无妨，将其作为物理公式，会用即可。
