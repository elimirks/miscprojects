impl Solution {
  pub fn min_steps(mut n: i32) -> i32 {
    if n == 1 {
      return 0;
    }

    let mut ops = 0;

    while (n & 1) == 0 {
      ops += 2;
      n >>= 1;
    }

    let mut factor = 3;
    while n != 1 {
      if n % factor == 0 {
        n /= factor;
        ops += factor;
      } else {
        factor += 2;

        if factor * factor > n {
          ops += n;
          break;
        }
      }
    }

    return ops;
  }
}
