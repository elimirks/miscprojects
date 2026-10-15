function permuteCat(a: Set<string>, b: Set<string>): Set<string> {
  let results: Set<string> = new Set();
  for (let i of a) {
    for (let j of b) {
      results.add(i + j);
      results.add(j + i);
    }
  }
  return results;
}

function wrap(a: Set<string>): Set<string> {
  let results: Set<string> = new Set();
  for (let i of a) {
    results.add('(' + i + ')');
  }
  return results;
}

function generateParenthesis(n: number): string[] {
  let dp: Set<string>[] = [
    new Set([]),
    new Set(['()']),
  ];

  for (let i = 2; i < n + 1; i++) {
    // 1. Wrapping previous permunations in extra parens
    let results: Set<string> = wrap(dp[i - 1]);
    // 2. All concatenations of 2 subproblems
    for (let j = 1; j <= i / 2; j++) {
      for (let sub of permuteCat(dp[j], dp[i - j])) {
        results.add(sub);
      }
    }
    dp.push(results);
  }

  return Array.from(dp[n]);
}
