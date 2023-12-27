pub fn gcd(a: i64, b: i64) -> i64 {
    let mut a = a.abs();
    let mut b = b.abs();

    while a != 0 { (a, b) = (b % a, a); }

    return b;
}

pub fn egcd(a: i64, b: i64) -> (i64, i64) {
    if b == 0 { return (a.signum(), 0); }

    let mut x = a.abs();
    let mut y = b.abs();

    let mut s = 0;
    let mut t = 1;

    let mut quot;

    while x != 0 {
        quot = b / a;
        (x, y) = (b - quot * a, a);
        (s, t) = (t - quot * s, s);
    }

    s *= a.signum();
    t = (y - s * a) / b;

    return (s, t);
}
