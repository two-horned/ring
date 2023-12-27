//use ring::gcd;
//use ring::euclid;

use criterion::{criterion_main, criterion_group, Criterion};

const FIRST: i128 = fib(170);
const SECOND: i128 = fib(180);

const fn fib(n: i128) -> i128 {
    if n < 0 { return 0; }

    let mut n = n;
    let mut a = 0;
    let mut b = 1;

    while 0 < n { (a, b) = (b, a+b); n -= 1; }

    return b;
}

fn quot_test(a: i128, b: i128) -> (i128, i128) {
    let q = b / a;
    let c = b - q * a;

    return (q, c);
}

fn rem_test(a: i128, b: i128) -> (i128, i128) {
    let q = b / a;
    let c = b % a;

    return (q, c);
}

fn smallquot(c: &mut Criterion) {
    let mut group = c.benchmark_group("small benchmark");
    group.bench_function("small quot test", |b| b.iter(|| { quot_test(FIRST, SECOND) } ));
    group.finish();
}

fn smallrem(c: &mut Criterion) {
    let mut group = c.benchmark_group("small benchmark");
    group.bench_function("small rem test", |b| b.iter(|| { rem_test(FIRST, SECOND) } ));
    group.finish();
}

criterion_group!(benches, smallquot, smallrem);
criterion_main!(benches);
