function countSeniors(details: string[]): number {
  let count = 0;
  for (let detail of details) {
    const age = 10 * (detail.charCodeAt(11) - 48) + (detail.charCodeAt(12) - 48);
    if (age > 60) count++;
  }
  return count;
}
