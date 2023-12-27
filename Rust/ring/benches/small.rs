use ring::gcd;
//use ring::euclid;

use criterion::{criterion_main, criterion_group, Criterion};

const FIRST: i128 = fib(21);

const fn fib(n: i128) -> i128 {
    if n < 0 { return 0; }

    let mut n = n;
    let mut a = 0;
    let mut b = 1;

    while 0 < n { (a, b) = (b, a+b); n -= 1; }

    return b;
}

fn ring_gcd_first(c: &mut Criterion) {
    let mut group = c.benchmark_group("gcd/Ring/a=fib(21)");
    group.bench_function("323", |b| b.iter(|| gcd(FIRST, fib(23))));
    group.finish();
}

criterion_group!(benches, ring_gcd_first);
criterion_main!(benches);
