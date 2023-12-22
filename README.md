# Ring
This project provides an efficent way
to calculate the gcd of two numbers.

## Greatest Common Divisor
To calculate the gcd efficiently,
we should not use the euclidean algorithm.
We will see below why the peformance is way
worse than what we show here.

To calculate the gcd we look
how much of the number `a` "overlaps",
when we try to "fit" it in number `b`
and vice versa.

Visualization:
```
b : - - - - - (5)
a : - - -     (3)

overlap?

b : - - - - -|- - - - -
a : - - -|- - -|- - -
             ^
cut here ----'

=> overlap of 1 (for fitting a in b)
=> overlap of 2 (for fitting b in a)

```

This means we calculate the `b mod a`,
to get the overlap of b.

We can simply substract this overlap
from a (`a - b mod a`) to get the overlap of a.


Because the `gcd` divides both of these numbers,
these overlaps need to be divisible by the gcd.
So we can apply the `gcd` to these overlaps 
(e.g. with recursion) till we don't get any overlaps
and we have a clear result (`gcd 0 x = |x|`).

This algorithm clearly divides `a` in two parts.
So when we dealt with two numbers `a` and `b`
with `b >= a` (without loss of generality), 
our initial problem was maximal of size `a` 
(we could linearily search all numbers from a to 1),
but now we are dealing with maximal half the steps
required (biggest part of two is exact half).

Applying this halfing of two numbers recursively
will mean, our problem has a worst case of less or equal than `log_2 a` steps (`O(log n)`),
and most of the time it will perform way better, because the average case
is far from always triggering this case of always halfing exactly 
(we could've tried to find a better fit for an upper bound but it's not necessary).

With the euclidean algorithm we don't reduce the
initial problem as fast (it's already easy to tell,
that our step case is decreasing much faster),
and the worst case is approaching `5 * log_10 a` (proof can be found online),
which is far worse, even if it is asymptotically equal.


## Extended GCD
With our new approach to find the gcd, we can try to
improve a way to find numbers `s` and `t`, so that
`gcd(a,b) = s*a + t*b`.

For this we have following cases for a and b:
- Either of them is zero, which will make finding s and t trivial.
- We find s and t by applying the recursion, which will find s and t
  of one gcd step further, which we can use to deduce our s and t:
  ```
  c := b mod a
  d := b div a

  gcd(a,b) = gcd(c, a - c) 
           = s_1 * c + t_1 * (a - c)
           = s_1 * (d*a - b) + t_1 * (b - (d+1) * a)
           = s_1 * d*a - s_1 * b + t_1 * b - t_1 * (d+1) * a
           = (t_1 - s_1) * b + s_1 * d*a - t_1 * (d+1) * a
           = ((s_1*d) - t_1*(d+1)) * a + (t_1 - s_1) * b
  ```

    Because we know, that the gcd will never end in a loop (for integer inputs),
    we know we can always find the s and t.

Because of the same reason, why our algorithm for the gcd is better than the euclidean,
we know this algorithm will perform better than using substution techniques
using the euclidean algorithm.
