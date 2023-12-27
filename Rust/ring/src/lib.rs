pub mod euclid;

pub fn gcd(a: i64, b: i64) -> i64 {
    let mut a = a.abs();
    let mut b = b.abs();

    while a != 0 {
        if b < a { (a,b) = (b,a); }
        else {
            b %= a;
            a -= b;
        }
    }

    return b;
}

pub fn egcd(a: i64, b: i64) -> (i64, i64) {
    if b == 0 { return (a.signum(), 0); }

    let mut x = a.abs();
    let mut y = b.abs();

    let mut s = 0;
    let mut t = 1;

    let mut temp;

    while x != 0 {
        if y < x {
            (x,y) = (y,x);
            (s,t) = (t,s);
        } else {
            temp = y / x;
            y -= temp * x;
            x -= y;

            temp *= s;
            s += temp - t;
            t -= temp;
        }
    }

    t *= b.signum();
    s = (y - t * b) / a;

    return (s, t);
}
