# Rust Implementation
The Rust implementation contains the gcd.

## Benchmarking

These benchmarking results are **heavily** dependent on
the word size of your system. In my case the word size is 64,
which makes values above the size of (2^32) behave in an unusual way.
```
just smaller than (2^127), euclid (gcd), a = fib(183), b = fib(184)
                        time:   [389.03 ns 389.19 ns 389.35 ns]
Found 7 outliers among 100 measurements (7.00%)
  2 (2.00%) high mild
  5 (5.00%) high severe

just smaller than (2^127), ring (gcd), a = fib(183), b = fib(184)
                        time:   [339.89 ns 340.03 ns 340.17 ns]
Found 4 outliers among 100 measurements (4.00%)
  2 (2.00%) high mild
  2 (2.00%) high severe

just smaller than (2^63), euclid (gcd), a = fib(33), b = fib(34)
                        time:   [338.55 ns 338.73 ns 338.89 ns]
Found 2 outliers among 100 measurements (2.00%)
  1 (1.00%) high mild
  1 (1.00%) high severe

just smaller than (2^63), ring (gcd), a = fib(33), b = fib(34)
                        time:   [217.14 ns 217.41 ns 217.75 ns]
Found 1 outliers among 100 measurements (1.00%)
  1 (1.00%) high mild

just smaller than (2^31), euclid (gcd), a = fib(33), b = fib(34)
                        time:   [132.53 ns 132.60 ns 132.68 ns]
Found 4 outliers among 100 measurements (4.00%)
  1 (1.00%) low severe
  2 (2.00%) high mild
  1 (1.00%) high severe

just smaller than (2^31), ring (gcd), a = fib(33), b = fib(34)
                        time:   [76.868 ns 76.919 ns 76.978 ns]
Found 7 outliers among 100 measurements (7.00%)
  3 (3.00%) high mild
  4 (4.00%) high severe
```

### Hardware Used
- AMD Ryzen 6800HS (5Ghz)
- 16GiB DDR5 RAM (6400Mhz)
