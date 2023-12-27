pub fn gcd(a: i128, b: i128) -> i128 {
    let mut a = a.abs();
    let mut b = b.abs();

    while a != 0 { (a, b) = (b%a, a); }

    return b;
}
