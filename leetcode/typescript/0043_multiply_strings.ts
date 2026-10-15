function add(num1: string, num2: string): string {
  let result = "";
  let carry = 0;
  for (let i = 0; i < num1.length || i < num2.length || carry != 0; i++) {
    let sum = carry;
    if (i < num1.length) sum += parseInt(num1[num1.length - i - 1]);
    if (i < num2.length) sum += parseInt(num2[num2.length - i - 1]);
    result = `${sum % 10}${result}`;
    carry = Math.floor(sum / 10);
  }
  return result;
}

function multiply(num1: string, num2: string): string {
  if (num1 == "0" || num2 == "0") return "0";

  let result = "0";

  for (let i = 0; i < num1.length; i++) {
    for (let j = 0; j < num2.length; j++) {
      let trailingZeros = '0'.repeat(i + j);
      let x = parseInt(num1[num1.length - i - 1]);
      let y = parseInt(num2[num2.length - j - 1]);
      result = add(`${x * y}${trailingZeros}`, result);
    }
  }

  return result;
}
