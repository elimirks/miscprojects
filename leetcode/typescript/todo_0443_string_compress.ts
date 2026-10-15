// TODO: https://leetcode.com/problems/string-compression/
function compress(chars: string[]): number {
  if (chars.length === 0) throw "Invalid input";
  let i = 1;
  let count: number | null = null;
  while (i < chars.length) {
    if (count !== null && chars[i - 2] == chars[i]) {
      count++;
      chars[i - 1] = "" + count;
    }
  }
  return chars.length;
}
