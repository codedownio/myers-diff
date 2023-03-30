
# Welcome to `myers-diff` [![Hackage](https://img.shields.io/hackage/v/myers-diff.svg)](https://hackage.haskell.org/package/myers-diff) ![myers-diff](https://github.com/codedownio/myers-diff/actions/workflows/ci.yml/badge.svg)

This is a fast Haskell implementation of the Myers text diff algorithm[^1]. It is heavily inspired by the Python version in [this post](https://blog.robertelder.org/diff-algorithm/), and should have the same `O(min(len(a), len(b)))` space complexity. (By contrast, the [Diff](https://hackage.haskell.org/package/Diff) package advertises `O(ab)` space complexity.) The implementation uses unboxed mutable vectors for performance.

This repo also can also build a couple other versions for benchmarking comparison, gated behind flags.

* `-funi_myers` will build the version from the [uni-util](https://hackage.haskell.org/package/uni-util-2.3.0.3/docs/Util-Myers.html) package.
* `-fdiff_myers` will use the [Diff](https://hackage.haskell.org/package/Diff) package.

[^1]: E. Myers (1986). "An O(ND) Difference Algorithm and Its Variations". Algorithmica. 1 (2): 251–266. CiteSeerX [10.1.1.4.6927](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927). doi:[10.1007/BF01840446](https://doi.org/10.1007%2FBF01840446). S2CID [6996809](https://api.semanticscholar.org/CorpusID:6996809).
