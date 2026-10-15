// For 10^0, there's only one one
let onesForPlace = [1];
for (let i = 1; i < 10; i++) {
  onesForPlace[i] = onesForPlace[i - 1] * 10 + Math.pow(10, i);
}

function countDigitOne(n: number): number {
  if (n === 0) return 0;
  if (n < 10) return 1;

  let powNum = Math.floor(Math.log10(n));
  let lowSignificance = n % Math.pow(10, powNum);
  let highSignificance = Math.floor(n / Math.pow(10, powNum));

  let result = 0;
  if (highSignificance === 1) {
    result += countDigitOne(lowSignificance) * (lowSignificance + 1);
  } else {
    result += countDigitOne(lowSignificance);
  }

  if (highSignificance == 1) {
    result += onesForPlace[powNum - 1] + 1;
  } else {
    result += onesForPlace[powNum - 1] * (Math.pow(10, powNum) + highSignificance);
  }

  return result;
}
