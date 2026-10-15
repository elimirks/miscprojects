function toMinutes(timePoint: string): number {
  let hours = parseInt(timePoint.substring(0, 2));
  let mins = parseInt(timePoint.substring(3));
  return hours * 60 + mins;
}

function findMinDifference(timePoints: string[]): number {
  if (timePoints.length < 2) throw "Invalid input";
  let times = timePoints.map(toMinutes).sort((a, b) => a - b);

  // Handle wrapping around the clock
  let res = Math.abs(times[0] - (times[times.length - 1] - 24 * 60));

  for (let i = 0; i + 1 < times.length; i++) {
    res = Math.min(res, Math.abs(times[i] - times[i + 1]));
  }
  return res;
}
