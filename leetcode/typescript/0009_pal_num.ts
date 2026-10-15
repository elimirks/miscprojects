// Assumes x >= 0
function reverse(x: number): number {
  let res = 0;
  while (x !== 0) {
    const pop = x % 10;
    x = Math.floor(x / 10);
    res = res * 10 + pop;
  }
  return res;
}

function isPalindrome(x: number): boolean {
  if (x < 0) return false;
  if (x < 10) return true;
  return reverse(x) == x;
}
