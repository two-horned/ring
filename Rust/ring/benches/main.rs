use ring::gcd;
use criterion::{criterion_group, criterion_main, Criterion};

fn fib(n: i128) -> i128 {
    if n < 0 { return 0; }

    let mut n = n;
    let mut a = 0;
    let mut b = 1;

    while 0 < n { (a, b) = (b, a+b); n -= 1; }

    return b;
}

fn euclid(a: i128, b: i128) -> i128 {
    let mut a = a.abs();
    let mut b = b.abs();

    while a != 0 { (a, b) = (b%a, a); }

    return b;
}

fn euclid_bench(c: &mut Criterion) {
    let x = fib(183);
    let y = fib(184);

    c.bench_function("just smaller than (2^127), euclid (gcd), a = fib(183), b = fib(184)", |b| {
        b.iter(|| {
            euclid(x, y);
        });
    });
}

fn ring_bench(c: &mut Criterion) {
    let x = fib(183);
    let y = fib(184);

    c.bench_function("just smaller than (2^127), ring (gcd), a = fib(183), b = fib(184)", |b| {
        b.iter(|| {
            gcd(x, y);
        });
    });
}

fn euclid_bench2(c: &mut Criterion) {
    let x = fib(91);
    let y = fib(92);

    c.bench_function("just smaller than (2^63), euclid (gcd), a = fib(33), b = fib(34)", |b| {
        b.iter(|| {
            euclid(x, y);
        });
    });
}

fn ring_bench2(c: &mut Criterion) {
    let x = fib(91);
    let y = fib(92);

    c.bench_function("just smaller than (2^63), ring (gcd), a = fib(33), b = fib(34)", |b| {
        b.iter(|| {
            gcd(x, y);
        });
    });
}


fn euclid_bench3(c: &mut Criterion) {
    let x = fib(33);
    let y = fib(34);

    c.bench_function("just smaller than (2^31), euclid (gcd), a = fib(33), b = fib(34)", |b| {
        b.iter(|| {
            euclid(x, y);
        });
    });
}

fn ring_bench3(c: &mut Criterion) {
    let x = fib(33);
    let y = fib(34);

    c.bench_function("just smaller than (2^31), ring (gcd), a = fib(33), b = fib(34)", |b| {
        b.iter(|| {
            gcd(x, y);
        });
    });
}

criterion_group!(benches, euclid_bench, ring_bench, euclid_bench2, ring_bench2, euclid_bench3, ring_bench3);
criterion_main!(benches);
