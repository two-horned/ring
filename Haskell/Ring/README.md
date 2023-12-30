# Haskell Implementation
The Haskell code contains
all in the project's introduced algorithms
and benchmarking capabilities.

# Benchmarking
It's required to use cabal and install the required
packages.

## Results
[Download the results here.](https://github.com/two-horned/ring/issues/3)

Inside the zip-archieves you will find generated html files that you can open
with the browser. It will take some time to render these files but afterwards
you will see a detailed report of my benchmarks. If you'd rather like to
inspect other file formats, you can also read the `csv`, `json` and `xml` formatted
reports.

### GCD
I have measured times exactly predicted by
my worst case analysis. Because we only use half
the amount of modulo opertations compared to
the Euclidean algorithm (which is the most expensive
opertation in it), we are experiencing a huge performance boost
of around 100% (when we get a spike / get close to the worst case)
and are otherwise never really slower but mostly faster.

### EGCD
Comparing the two egcds is a bit more complicated.
We still have an edge over the Euclidean version
when we are reaching "spikes" in the tests.
The advantage isn't as significant anymore,
only around 70-80% performance boost for the worst cases
(still a great improvement!).

For other cases we are never really slower than the
Euclidean version but tend to use an equal amount of time.

One reason why we didn't improve as much as in the gcd
comparison is possibly due to the Haskell compiler (we used `ghc`)
not being able to interprete more detailed algorithms that well.

In theory, if we rerun the tests in an imperative Programming language,
where we can make use of variables and selfassignments
of variables (see imperative pseudocode),
explicitly, we would get even better results,
because we don't need to rely on the interpretation
ability of the Haskell compiler anymore.

PS: As it turns out, this is exactly true.
Take a look at the [Rust implementation](https://github.com/two-horned/ring/tree/main/Rust/ring) and their benchmarking results!

### Hardware used
- CPU: AMD Ryzen 6800HS (5GHz),
- RAM: 16GiB DDR5 (6400MHz)
