use ring::gcd;
use ring::euclid;

use criterion::BenchmarkId;
use criterion::{criterion_main, criterion_group, Criterion};

const FIRST: i128 = fib(21);
const SECOND: i128 = fib(57);
const THIRD: i128 = fib(91);

fn testlist() -> [i128; 183] {
    let mut r = [0; 183]; 
    for i in 0 .. 182 {
        r[i] = fib(i as i128 +1);
    }
    return r;
}

const fn fib(n: i128) -> i128 {
    if n < 0 { return 0; }

    let mut n = n;
    let mut a = 0;
    let mut b = 1;

    while 0 < n { (a, b) = (b, a+b); n -= 1; }

    return b;
}

fn ring_gcd_first(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("gcd/Ring/a=fib(21)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                gcd(FIRST, i); 
            });
        });
    }
    group.finish();
}

fn ring_gcd_second(c: &mut Criterion) {
    let testlist = testlist();
    let mut group = c.benchmark_group("gcd/Ring/a=fib(57)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                gcd(SECOND, i); 
            });
        });
    }
    group.finish();
}

fn ring_gcd_third(c: &mut Criterion) {
    let testlist = testlist();
    let mut group = c.benchmark_group("gcd/Ring/a=fib(91)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                gcd(THIRD, i); 
            });
        });
    }
    group.finish();
}

fn euclid_gcd_first(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("gcd/Euclidean/a=fib(21)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                euclid::gcd(FIRST, i); 
            });
        });
    }
    group.finish();
}

fn euclid_gcd_second(c: &mut Criterion) {
    let testlist = testlist();
    let mut group = c.benchmark_group("gcd/Euclidean/a=fib(57)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                euclid::gcd(SECOND, i); 
            });
        });
    }
    group.finish();
}

fn euclid_gcd_third(c: &mut Criterion) {
    let testlist = testlist();
    let mut group = c.benchmark_group("gcd/Euclidean/a=fib(91)");
    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| { 
                euclid::gcd(THIRD, i); 
            });
        });
    }
    group.finish();
}

criterion_group!(benches, ring_gcd_first, ring_gcd_second, ring_gcd_third, euclid_gcd_first, euclid_gcd_second, euclid_gcd_third);
criterion_main!(benches);
