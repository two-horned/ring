# Rust Implementation
The Rust implementation contains the gcd.

## Benchmarking

These benchmarking results are **heavily** dependent on
the word size of your system. In my case the word size is 64,
which makes values above the size of (2^{63} - 1) behave in an unusual way (and below -2^{63}).


### Results
[Download tests results here.](https://github.com/two-horned/ring/issues/5)

The results just met my predictions. 
Because we can control the assignments of variables by ourselves,
we achieve a better control flow compared to the [Haskell implementation](https://github.com/two-horned/ring/tree/main/Haskell/Ring).

The GCD was already fully optimized in the Haskell implementation, and
we already had a 100% performance increase for all worst cases and
(almost) never lacked behind the Euclidean version in other cases (environment relatated deviations are expected).

The EGCD however showed in this test results improved a lot.
Not only do we get a 100% performance increase for all worst cases,
but we never lack behind in any other case as well. Since the
selfassignments used in this algorithm never slowed us down
compared to the new constant generation in Haskell.

#### Plotting the results
##### Fibonacci

|         | GCD | EGCD
| ------- | --- | ----
| Fib(21) | ![gcd_fib(21)](https://github.com/two-horned/ring/assets/95277266/01804f3c-d131-44ad-abb8-01d9ad4b98ab) | ![egcd_fib(21)](https://github.com/two-horned/ring/assets/95277266/f2ec6e34-1c31-4515-a30a-855d58844216)
| Fib(57) | ![gcd_fib(57)](https://github.com/two-horned/ring/assets/95277266/e76f88a3-cb3e-4d8d-9651-6cdb3e11c094) | ![egcd_fib(57)](https://github.com/two-horned/ring/assets/95277266/af0b9933-93f9-447a-abb1-0025577c71e8)
| Fib(91) | ![gcd_fib(91)](https://github.com/two-horned/ring/assets/95277266/65d78401-804f-4fce-baf4-95267f7f1a40) | ![egcd_fib(91)](https://github.com/two-horned/ring/assets/95277266/aed2b767-54f7-436e-b32b-6badb6511a76)

##### Leonardo

|         | GCD | EGCD
| ------- | --- | ----
| Leo(21) | ![gcd_leo(21)](https://github.com/two-horned/ring/assets/95277266/56b7ae96-f957-481e-9052-3a9315614a5b) | ![egcd_leo(21)](https://github.com/two-horned/ring/assets/95277266/d271e2e9-9716-49da-b8ab-7e06c3757af2)
| Leo(57) | ![gcd_leo(57)](https://github.com/two-horned/ring/assets/95277266/12c079d4-1683-42ef-8c95-bb25dd491eac) | ![egcd_leo(57)](https://github.com/two-horned/ring/assets/95277266/ee6b8dc9-4b38-4214-a691-5dace2af7fed)
| Leo(83) | ![gcd_leo(83)](https://github.com/two-horned/ring/assets/95277266/a85392d2-3236-42ad-93db-06c1d3250d92) | ![egcd_leo(83)](https://github.com/two-horned/ring/assets/95277266/b66b47df-c0b9-4de2-a33f-abeaf264c5ed)

### Hardware Used
- CPU: AMD Ryzen 6800HS (5Ghz)
- RAM: 16GiB DDR5 (6400Mhz)
