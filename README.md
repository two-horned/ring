# Ring
This project provides an efficent way
to calculate the gcd of two numbers.

## Greatest Common Divisor
To calculate the gcd efficiently,
we should not use the euclidean algorithm.
We will see below why the (worst case) peformance is way
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

### Runtime analysis
The runtime of my algorithm can be derived
in this manner. We solve, which smallest numbers
with `b >= a` (without loss of generality) will
produce an amount of steps required when running the algorithm.

Steps | a | b
---
1     | 0                   | 1
2     | 1                   | 1
3     | 2                   | 3
4     | 5                   | 7
5     | 12                  | 17
n     | sum of values above | above a + sum of values above

We can clearly see, the only way to reach a new number
of steps is by defining a new `a` as the sum a previous
line due to us passing `gcd(a - r, r)` to reduce the problem
(`(a - r) + r = a`). The smallest value to generate the needed remainder
and it's inverse is `a` because we assume that's the smaller of the
two numbers. Our worst case lies between `[1/2 log_ϕ (min {a,b}), log_2 (min{a,b})]`,
which is around twice as fast as the Euclidean algorithm.

### Pseudocode
#### Recursive
```java
gcd(a, b) {
  a = |a|
  b = |b|
  
  if (b < a) 
    return gcd(b, a)
  
  if (a == 0)
    return b
  
  r = b % a                // remainder of b / a
  
  return gcd(a - r, r)
}
```

#### Iterative
```java
gcd(a, b) { 
  a = |a|
  b = |b|

  while (a != 0) {
    if (b < a) {
        (a, b) = (b, a)        // Usage of parallel assignment.
    } else {
      b %= a                   // Usage of selfassignment.
      a -= b
    }
  }
  
  return b
}
```

## Extended GCD
With our new approach to find the gcd, we can try to
improve a way to find numbers `s` and `t`, so that
`gcd(a,b) = s*a + t*b`.

For this we have following cases for a and b:
- Either of them is zero, which will make finding s and t trivial.
- We find s and t by applying the recursion, which will find s and t
  of one gcd step further, which we can use to deduce our next s and t:
  ```
  r := b rem a  (remainder)
  q := b quot a (quotient)

  gcd(a,b) = gcd(a - r, r) 
           = s * (a - r) + t * r
           = s * (a - (b - q*a)) + t * (b - q*a)
           = s * ((q+1) * a - b) + t * (b - q*a)
           = ((s - t) * q + s) a + (t - s) * b
                      ^               ^
  "our next s" -------'               '
                                      ' 
  "our next t" -----------------------'
  ```

    Because we know, that the gcd will never end in a loop (for integer inputs),
    we know we can always find the s and t.

Because of the same reason, why our algorithm for the gcd is better than the euclidean,
we know this algorithm will perform better than using substution techniques
using the euclidean algorithm.

### Pseudocode

#### Recursive

It's very easy to find the (head-)recursive solution.
We simply the subsitution rules from above.

```java
egcd(a, b) {
  a = |a|
  b = |b|
  
  if (b < a) {
    (t, s) = egcd(b, a)
    return (s, t)
  }
  
  if (a == 0)
    return (0, 1)
  
  q = b / a                           // q: quotient of b / a
  r = b % a                           // r: remainder of b / a
  
  (ss, tt) = egcd(a - r, r)           // intermediate result
  return ((ss - tt) * q + ss, tt - ss)
}
```

#### Iterative

This solution requires you to wrap your head around
and try to find the meaning of what it means if we
hit the case of `a = 0`. You could reason similar to the
extended euclidean algorithm, and you'll notice that
the coefficient dedicated to `b` will always represent the `t`
we are looking for. If you have further questions, you can
contact me and we can discuss this in more detail.

```java
egcd(a, b) {
  if (b == 0)                // Edge case. We don't want to
    return (signum(a), 0)    // divide with zero in the future.

  x = |a|
  y = |b|

  s = 0
  t = 1

  while (x != 0) {
    if y < x {
      (x,y) = (y,x)
      (s,t) = (t,s)
    } else {
      temp = y / x       // temp: quotient of y / x
      y -= temp * x
      x -= y

      temp *= s         // temp: not the quotient anymore.
      s += temp - t
      t -= temp
    }
  }

  t *= signum(b)
  s = (y - t * b) / a

  return (s, t)
}
```
