# Rust Implementation
The Rust implementation contains the gcd.

## Benchmarking

These benchmarking results are **heavily** dependent on
the word size of your system. In my case the word size is 64,
which makes values above the size of (2^{63} - 1) behave in an unusual way (and below -2^{63}).


### Results
[Test results for fibonacci numbers.](https://github.com/two-horned/ring/issues/5)

The results just met my predictions. 
Because we can control the assignments of variables by ourselves,
we achieve a better control flow compared to the Haskell implementation.

The GCD was already fully optimized in the Haskell implementation, and
we already had a 100% performance increase for all worst cases and
never lacked behind the Euclidean version in other cases.

The EGCD however showed in this test results improved a lot.
Not only do we get a 100% performance increase for all worst cases,
but we never lack behind in any other case as well. Since the
selfassignments used in this algorithm never slowed us down
compared to the new constant generation in Haskell.

### Hardware Used
- AMD Ryzen 6800HS (5Ghz)
- 16GiB DDR5 RAM (6400Mhz)
