//use ring::gcd;
//use ring::euclid;

use criterion::{criterion_main, criterion_group, Criterion};

const FIRST: i128 = 3;
const SECOND: i128 = 2^56 + 1;

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
