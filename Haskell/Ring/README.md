# Haskell Implementation
The Haskell code contains
all in the project's introduced algorithms
and benchmarking capabilities.

# Benchmarking
It's required to use cabal and install the required
packages.

I have measured times exactly predicted by
my worst case analysis. Because we only use half
the amount of modulo opertations compared to 
the euclidean algorithm (which is the most expensive
opertation in it), we are experiencing a huge performance boost
of around 100%.

**Important** is that you should alternate between the order of tests,
because the Java VM runs warm when performing tasks on the first
algorithm and therefore the second has some advantage.

## Results
```
benchmarking gcd/Ring, gcd(a,b)
time                 34.09 ms   (16.36 ms .. 47.84 ms)
                     0.653 R²   (0.217 R² .. 0.889 R²)
mean                 72.76 ms   (58.83 ms .. 83.93 ms)
std dev              22.76 ms   (19.64 ms .. 25.44 ms)
variance introduced by outliers: 82% (severely inflated)

benchmarking gcd/Euclid, gcd(a,b)
time                 58.39 ms   (57.76 ms .. 59.07 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 59.04 ms   (58.70 ms .. 59.66 ms)
std dev              830.8 μs   (488.9 μs .. 1.173 ms)
```

### Hardware used
- Cpu: AMD Ryzen 6800HS (5GHz),
- RAM: 16GiB DDR5 (6400MHz)
