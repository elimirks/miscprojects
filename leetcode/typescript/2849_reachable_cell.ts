function isReachableAtTime(sx: number, sy: number, fx: number, fy: number, t: number): boolean {
  let deltaX = Math.abs(sx - fx);
  let deltaY = Math.abs(sy - fy);
  if (deltaX == 0 && deltaY == 0) {
    return t != 1;
  }
  return Math.max(deltaX, deltaY) <= t;
}
