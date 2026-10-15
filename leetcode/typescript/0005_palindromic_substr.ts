function isPalindrome(s: string): boolean {
  if (s.length <= 1) return true;

  if (s[0] === s[s.length - 1]) {
    return isPalindrome(s.substring(1, s.length - 1));
  } else {
    return false;
  }
}

function longestOddPalindrome(s: string): string {
  let longest = '';
  let origin = 0;
  let radius = 0;

  while (origin + radius <= s.length) {
    let sub = s.substring(origin - radius, origin + radius + 1);
    if (isPalindrome(sub)) {
      if (sub.length > longest.length) {
        longest = sub;
      }
      if (origin - radius > 0) {
        radius++;
      } else {
        origin++;
      }
    } else {
      origin++;
    }
  }

  return longest;
}

function longestEvenPalindrome(s: string): string {
  let longest = '';
  let origin = 0;
  let radius = 0;

  while (origin + radius + 1 <= s.length) {
    let sub = s.substring(origin - radius, origin + radius + 2);
    if (isPalindrome(sub)) {
      if (sub.length > longest.length) {
        longest = sub;
      }
      if (origin - radius > 0) {
        radius++;
      } else {
        origin++;
      }
    } else {
      origin++;
    }
  }

  return longest;
}

function longestPalindrome(s: string): string {
  let even = longestEvenPalindrome(s);
  let odd = longestOddPalindrome(s);

  if (even.length > odd.length) {
    return even;
  } else {
    return odd;
  }
}
