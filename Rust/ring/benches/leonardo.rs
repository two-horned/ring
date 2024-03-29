use std::time::Duration;

use ring::{gcd, egcd};
use ring::euclid;

use criterion::BenchmarkId;
use criterion::{criterion_main, criterion_group, Criterion};

const FIRST: i64 = leo(21);
const SECOND: i64 = leo(57);
const THIRD: i64 = leo(83);

const WTIME: Duration = Duration::from_millis(500);
const MTIME: Duration = Duration::from_secs(2);

fn testlist() -> [i64; 89] {
    let mut r = [0; 89];
    for i in 0 .. 89 {
        r[i] = leo(i as i64 + 1);
    }
    return r;
}

const fn leo(n: i64) -> i64 {
    if n < 0 { return 0; }

    let mut n = n;
    let mut a = -1;
    let mut b = 1;

    while 0 < n { (a, b) = (b, a + b + 1); n -= 1; }

    return b;
}

fn ring_gcd_first(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("gcd/Ring/a=leo(21)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

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

    let mut group = c.benchmark_group("gcd/Ring/a=leo(57)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

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

    let mut group = c.benchmark_group("gcd/Ring/a=leo(83)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

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

    let mut group = c.benchmark_group("gcd/Euclidean/a=leo(21)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

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

    let mut group = c.benchmark_group("gcd/Euclidean/a=leo(57)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

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

    let mut group = c.benchmark_group("gcd/Euclidean/a=leo(83)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                euclid::gcd(THIRD, i);
            });
        });
    }
    group.finish();
}

fn ring_egcd_first(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Ring/a=leo(21)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                egcd(FIRST, i);
            });
        });
    }
    group.finish();
}

fn ring_egcd_second(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Ring/a=leo(57)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                egcd(SECOND, i);
            });
        });
    }
    group.finish();
}

fn ring_egcd_third(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Ring/a=leo(83)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                egcd(THIRD, i);
            });
        });
    }
    group.finish();
}

fn euclid_egcd_first(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Euclidean/a=leo(21)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                euclid::egcd(FIRST, i);
            });
        });
    }
    group.finish();
}

fn euclid_egcd_second(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Euclidean/a=leo(57)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                euclid::egcd(SECOND, i);
            });
        });
    }
    group.finish();
}

fn euclid_egcd_third(c: &mut Criterion) {
    let testlist = testlist();

    let mut group = c.benchmark_group("egcd/Euclidean/a=leo(83)");
    group.warm_up_time(WTIME);
    group.measurement_time(MTIME);

    for i in testlist.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(i), i, |b, &i| {
            b.iter(|| {
                euclid::egcd(THIRD, i);
            });
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    ring_gcd_first,
    ring_gcd_second,
    ring_gcd_third,
    euclid_gcd_first,
    euclid_gcd_second,
    euclid_gcd_third,
    ring_egcd_first,
    ring_egcd_second,
    ring_egcd_third,
    euclid_egcd_first,
    euclid_egcd_second,
    euclid_egcd_third,
    );
criterion_main!(benches);
