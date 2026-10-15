function computeArea(ax1: number, ay1: number, ax2: number, ay2: number, bx1: number, by1: number, bx2: number, by2: number): number {
  let overlapX = 0;
  let overlapY = 0;

  // Case 1: A is encompassing b on x
  if (ax1 <= bx1 && ax2 >= bx2) {
    overlapX = bx2 - bx1;
  // Case 2: b encompasses a
  } else if (ax1 >= bx1 && ax2 <= bx2) {
    overlapX = ax2 - ax1;
  // Case 3: right edge of a is inside b
  } else if (ax2 >= bx1 && ax2 <= bx2) {
    overlapX = ax2 - bx1;
  // Case 4: right edge of b is inside a
  } else if (ax1 <= bx2 && ax2 >= bx2) {
    overlapX = bx2 - ax1;
  }

  // Case 1: A is encompassing b on y
  if (ay1 <= by1 && ay2 >= by2) {
    overlapY = by2 - by1;
  // Case 2: b encompasses a
  } else if (by1 <= ay1 && by2 >= ay2) {
    overlapY = ay2 - ay1;
  // Case 3: right edge of a is inside b
  } else if (ay2 >= by1 && ay2 <= by2) {
    overlapY = ay2 - by1;
  // Case 4: right edge of b is inside a
  } else if (by2 >= ay1 && by2 <= ay2) {
    overlapY = by2 - ay1;
  }

  return (ax2 - ax1) * (ay2 - ay1) + (bx2 - bx1) * (by2 - by1) - overlapX * overlapY;
}
