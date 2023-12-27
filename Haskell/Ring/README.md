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
[Testing fibonacci numbers against eachother (36MiB HTML).](https://github.com/two-horned/ring/issues/3)

### Hardware used
- Cpu: AMD Ryzen 6800HS (5GHz),
- RAM: 16GiB DDR5 (6400MHz)
