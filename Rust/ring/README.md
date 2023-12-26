# Rust Implementation
The Rust implementation contains the gcd.

## Benchmarking

```
euclid (gcd), a = fib(183), b = fib(184)
                        time:   [386.77 ns 387.04 ns 387.34 ns]
                        change: [+1.7492% +1.8867% +2.0066%] (p = 0.00 < 0.05)
                        Performance has regressed.
Found 6 outliers among 100 measurements (6.00%)
  5 (5.00%) high mild
  1 (1.00%) high severe

ring (gcd), a = fib(183), b = fib(184)
                        time:   [339.59 ns 339.97 ns 340.37 ns]
                        change: [+0.1243% +0.2426% +0.3588%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 14 outliers among 100 measurements (14.00%)
  13 (13.00%) high mild
  1 (1.00%) high severe

euclid (gcd), a = fib(33), b = fib(34)
                        time:   [135.79 ns 135.97 ns 136.15 ns]
Found 6 outliers among 100 measurements (6.00%)
  1 (1.00%) low severe
  3 (3.00%) low mild
  2 (2.00%) high mild

ring (gcd), a = fib(33), b = fib(34)
                        time:   [75.672 ns 75.725 ns 75.806 ns]
Found 8 outliers among 100 measurements (8.00%)
  3 (3.00%) high mild
  5 (5.00%) high severe
```

### Hardware Used
- AMD Ryzen 6800HS (5Ghz)
- 16GiB DDR5 RAM (6400Mhz)
