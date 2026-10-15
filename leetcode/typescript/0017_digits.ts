let mapping: Map<string, string> = new Map([
  ["2", "abc"],
  ["3", "def"],
  ["4", "ghi"],
  ["5", "jkl"],
  ["6", "mno"],
  ["7", "pqrs"],
  ["8", "tuv"],
  ["9", "wxyz"],
]);

function letterCombinations(digits: string): string[] {
  if (digits.length == 0) return [];

  let results: string[] = [];
  let stage: string = mapping.get(digits[0]) || "";

  if (digits.length == 1) {
    for (let i = 0; i < stage.length; i++) {
      results.push(stage[i]);
    }
    return results;
  }

  let rec = letterCombinations(digits.substring(1));

  for (let i = 0; i < stage.length; i++) {
    let c = stage[i];

    for (let s of rec) {
      results.push(c + s);
    }
  }
  return results;
}
