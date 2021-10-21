use std::{f64, mem, fmt, cmp};

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

fn m256d_str(vec: __m256d) -> String {
    let unpacked: (f64, f64, f64, f64) = unsafe { mem::transmute(vec) };
    // Reverse order because endians!
    format!("<{}, {}, {}, {}>", unpacked.3, unpacked.2, unpacked.1, unpacked.0)
}

fn m256d_mandle(xs: __m256d, ys: __m256d) -> __m256d {
    let zsquared = _mm256_add_pd(
        _mm256_add_pd(_mm256_mul_pd(xs, xs), _mm256_mul_pd(ys, ys)),
        _mm256_mul_pd(_mm256_mul_pd(xs, ys), _mm256_set_pd(2.0, 2.0, 2.0, 2.0))
    );
}

fn main() {
    unsafe {
        let a: __m256d = _mm256_set_pd(1.0, 2.0, 3.0, 4.0);
        let b: __m256d = _mm256_set_pd(5.0, 1.0, 1.0, 3.0);
        let c = _mm256_mul_pd(a, b);

        println!("{}", m256d_str(c));
    };

    println!("Hello, world!");
}
