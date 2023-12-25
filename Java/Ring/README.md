# Java Implementation
The java code contains a comparison
between the method I introduced
as `ring` method and the classical euclidean method,
to calcutate the gcd.

# Testing
It's required to use JUnit 5 as testing suite.

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
Notice, how we can't really use bigger numbers than I used for testing,
because of buffer overlows and BigInteger type in Java is a bit 
tedious to work with. Still a comparison is still well performable
if we do enough iterations.

### GCD
- a = fib(44)
- b = fib(45)
- iterations = 10 Mio.

Method | Euclidean | "Ring"
--- | --- | ---
Avg. Time | 8.66s | 4.38s

### EGCD
Method | Euclidean | "Ring"
--- | --- | ---
Avg. Time | 4.86s | 3.63s

- a = fib(23)
- b = fib(24)
- iterations = 10 Mio.

### Hardware used
- Cpu: AMD Ryzen 6800HS (5GHz),
- RAM: 16GiB DDR5 (6400MHz)
