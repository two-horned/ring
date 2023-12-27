# Haskell Implementation
The Haskell code contains
all in the project's introduced algorithms
and benchmarking capabilities.

# Benchmarking
It's required to use cabal and install the required
packages.


**Important** is that you should alternate between the order of tests,
because the Java VM runs warm when performing tasks on the first
algorithm and therefore the second has some advantage.

## Results
[Testing fibonacci numbers against eachother (34MiB HTML).
Testing leonardo numbers against eachother (33MiB HTML).](https://github.com/two-horned/ring/issues/3)

### GCD
I have measured times exactly predicted by
my worst case analysis. Because we only use half
the amount of modulo opertations compared to 
the euclidean algorithm (which is the most expensive
opertation in it), we are experiencing a huge performance boost
of around 100%.

### EGCD
Comparing the two egcds is a bit more complicated.
We still have an edge over the Euclidean version
when we are reaching "spikes", "high points" in the tests.
The advantage isn't as significant anymore, only around 50% performance boost.

The drawback is, that more general cases need around 10-20% more time.

Reason for this kind of resul is, that the algorithm at it's core is way
more complex than the Euclidean version. So when the compiler was quick to
optimize some lines of code in the (still) simple version of the gcd, it's not
quiet there yet with the egcd. 

We are probably losing a lot of time because of new constants being all the time,
(which are hold by the go-tailrecursive-worker-function) whereas the Euclidean doesn't
need to do that, because one value is passed unchanged every time.

In theory, if we rerun the tests in an imperative Programming language,
where we can make use of variables and selfassignments of variables (see imperative pseudocode),
we would get even better results.

### Hardware used
- Cpu: AMD Ryzen 6800HS (5GHz),
- RAM: 16GiB DDR5 (6400MHz)
