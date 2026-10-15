function getSum(a: number, b: number): number {
  while (a != 0 && b != 0) {
    let carry = a & b;
    a &= ~carry;
    b &= ~carry;
    a = a + b;
    b = carry << 1;
  }
  return (a == 0) ? b : a;
}
