pub mod euclid;

pub fn gcd(a: i128, b: i128) -> i128 {
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
