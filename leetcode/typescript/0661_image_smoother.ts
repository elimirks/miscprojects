function smoothIdx(img: number[][], x: number, y: number): number {
  const maxX = Math.min(img.length - 1, x + 1);
  const minX = Math.max(0, x - 1);
  const maxY = Math.min(img[0].length - 1, y + 1);
  const minY = Math.max(0, y - 1);

  let sum = 0;
  for (let xi = minX; xi <= maxX; xi++) {
    for (let yi = minY; yi <= maxY; yi++) {
      sum += img[xi][yi];
    }
  }
  const count = (maxX + 1 - minX) * (maxY + 1 - minY);
  return Math.floor(sum / count);
}

function imageSmoother(img: number[][]): number[][] {
  let newImg: number[][] = [];
  for (let x = 0; x < img.length; x++) {
    newImg.push([]);
    for (let y = 0; y < img[0].length; y++) {
      newImg[x][y] = smoothIdx(img, x, y);
    }
  }
  return newImg;
}
