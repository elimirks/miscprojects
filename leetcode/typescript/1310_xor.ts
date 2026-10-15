function xorQueries(arr: number[], queries: number[][]): number[] {
  for (let i = 1; i < arr.length; i++) {
    arr[i] ^= arr[i - 1];
  }
  let res: number[] = [];
  for (let [left, right] of queries) {
    if (left == 0) {
      res.push(arr[right]);
    } else {
      res.push(arr[right] ^ arr[left - 1]);
    }
  }
  return res;
}
