# Fibonacci Benchmarks

Tiny program to compare the computation of n-th element of the Fibonacci sequence by:

* Naive recursive computation,
* Famous implementation using `zipWith` and infinite list,
* Forward DP: successively taking sums of two terms for n-times,
* Logarithmic DP: using the dependence between F(2n+1) and (F(n), F(n+1)),
* Memoised recursion, using IntMap and STArray as a cache,
* Directly computing general terms in a simple extension field Q(√5),
* Calculating n-th power of matrix [[0,1],[1,1]] and multiply to [0,1],
  both in naive computation and using diagonalisation,

The method of logarithmic recursion is communicated by @[n_vip](https://twitter.com/n_vip) and translated from [C++ version](http://ideone.com/H906ow).

The results are linked below:

* [Benchmark result (Optimised  with `-threaded -O2`)](https://raw.githubusercontent.com/konn/fibs/master/bench.html)
* [Benchmark result (No optimisation: `-O0`)](https://raw.githubusercontent.com/konn/fibs/master/bench-no-opts.html)

As a total, logarithmic DP is asymptotically fastest and matrix power is the second.

The `zipWith`-implementation does surprisingly well, even if it is expected to require extra burden by indexing.
One might suspects that this is due to the stream fusion optimisation mechanism, which eliminates intermediate terms.
But even if we add a hand-written version without fusion, it performs almost similar as GHC's builtin `zipWith` version.
Indeed, both version of `zipWith` is in linear time, but the coefficient is surprisingly small, which becomes apparent n > 700000.
Please let me know why zipWith version have such a slow growth.
