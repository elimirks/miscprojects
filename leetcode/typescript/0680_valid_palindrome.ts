function isPalindrome(s: string, dropped: boolean): boolean {
  if (s.length <= 1) return true;
  if (s[0] == s[s.length - 1]) {
    return isPalindrome(s.slice(1, s.length - 1), dropped);
  } else if (dropped) {
    return false;
  } else {
    return isPalindrome(s.slice(1), true) || isPalindrome(s.slice(0, s.length - 1), true);
  }
}

function validPalindrome(s: string): boolean {
  return isPalindrome(s, false);
}
