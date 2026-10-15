function canMakeSquareArray(grid: string[][]): boolean {
  let numRepr: number[] = [];
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      numRepr.push(grid[i][j] == "B" ? 1 : -1);
    }
  }

  return Math.abs(numRepr[0] + numRepr[1] + numRepr[3] + numRepr[4]) >= 2 ||
    Math.abs(numRepr[1] + numRepr[2] + numRepr[4] + numRepr[5]) >= 2 ||
    Math.abs(numRepr[3] + numRepr[4] + numRepr[6] + numRepr[7]) >= 2 ||
    Math.abs(numRepr[4] + numRepr[5] + numRepr[7] + numRepr[8]) >= 2;
}

// Bitmask approach
function canMakeSquare(grid: string[][]): boolean {
  let numRepr: number = 0;
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      numRepr <<= 1;
      if (grid[i][j] == "B") numRepr += 1;
    }
  }

  function maskCheck(shift: number): boolean {
    let mask = 0b11011;
    let bits = mask & (numRepr >> shift);
    // Counts 1s in a 5 bit number
    return ((bits * 0x0421) & 0x1249) % 7 != 2;
    // Alternatively
    //return !(bits == 0b00011 || bits == 0b01001 || bits == 0b01010 || bits == 0b10001 || bits == 0b10010 || bits == 0b11000);
  }

  return maskCheck(0) || maskCheck(1) || maskCheck(3) || maskCheck(4);
}
