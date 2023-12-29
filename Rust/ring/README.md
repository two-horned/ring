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

#### Plotting the results
##### Fibonacci

|         | GCD | EGCD
| ------- | --------- | ----
| Fib(21) | ![gcd_fib(21)](https://github.com/two-horned/ring/assets/95277266/01804f3c-d131-44ad-abb8-01d9ad4b98ab) | ![egcd_fib(21)](https://github.com/two-horned/ring/assets/95277266/f2ec6e34-1c31-4515-a30a-855d58844216)
| Fib(57) | ![gcd_fib(57)](https://github.com/two-horned/ring/assets/95277266/e76f88a3-cb3e-4d8d-9651-6cdb3e11c094) | ![egcd_fib(57)](https://github.com/two-horned/ring/assets/95277266/af0b9933-93f9-447a-abb1-0025577c71e8)
| Fib(91) | ![gcd_fib(91)](https://github.com/two-horned/ring/assets/95277266/65d78401-804f-4fce-baf4-95267f7f1a40) | ![egcd_fib(91)](https://github.com/two-horned/ring/assets/95277266/aed2b767-54f7-436e-b32b-6badb6511a76)

### Hardware Used
- AMD Ryzen 6800HS (5Ghz)
- 16GiB DDR5 RAM (6400Mhz)
