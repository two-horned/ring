# Java Implementation
The java code contains a comparison
between the method I introduced
as `ring` method and the classical euclidean method.

This is because Java is easier to show performance
benefits compared to Haskell because lazy evaluation
and other compiler "tricks" Haskell does, mess with
with benchmarking results in a way, we don't expect.

# Testing
It's required to use JUnit 5 as testing suite.
I have measured times were exactly predicted by
my worst case analysis. Because we only use half
the amount of modulo opertations compared to 
the euclidean algorithm (which is the most expensive
opertation in it), we are experiencing a huge performance boost
of around 100%.

**Important** is that you should alternate between the order of tests,
because the Java VM runs warm when performing tasks on the first
algorithm and therefore the second has some advantage.

Method | Euclidean | "Ring"
--- | --- | ---
Avg. Time | 8.66s | 4.3s
