function minStepsRec(n: number, bufSize: number = 1, clipboardSize: number = 0): number {
  if (bufSize == n) return 0;
  if (bufSize > n) return Infinity;

  // starting case
  if (clipboardSize == 0) {
    return 2 + minStepsRec(n, bufSize + 1, 1);
  } else {
    if (clipboardSize == bufSize) {
      return 1 + minStepsRec(n, bufSize + clipboardSize, clipboardSize); // paste
    } else {
      return 1 + Math.min(
        minStepsRec(n, bufSize, bufSize), // copy
        minStepsRec(n, bufSize + clipboardSize, clipboardSize), // paste
      );
    }
  }
}

function minStepsSemiOpt(n: number): number {
  if (n == 1) return 0;

  const factors = factorize(n);
  if (factors.length == 1) return n;

  let ops: number = 0;
  for (let i = 0; i < factors.length; i++) {
    ops += factors[i];
  }
  return ops;
}

// Worst case runtime: O(sqrt(n))
// Can be improved by skipping more `i` indices
function factorize(n: number): number[] {
  let factors: number[] = [];

  while (n % 2 == 0) {
    factors.push(2);
    n /= 2;
  }

  let i = 3;
  while (n != 1) {
    if (n % i == 0) {
      n /= i;
      factors.push(i);
    } else {
      i += 2;
      if (i * i > n) {
        factors.push(n);
        break;
      }
    }
  }

  return factors;
}

// O(sqrt(n)) runtime since we only iterate up to sqrt(n)
// O(1) memory
function minSteps(n: number): number {
  if (n == 1) return 0;

  let ops: number = 0;

  while ((n & 1) == 0) {
    ops += 2;
    n >>= 1;
  }

  let factor = 3;
  while (n != 1) {
    if (n % factor == 0) {
      n /= factor;
      ops += factor;
    } else {
      factor += 2;
      if (factor * factor > n) {
        ops += n;
        break;
      }
    }
  }

  return ops;
}
