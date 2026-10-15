function canFinish(numCourses: number, prerequisites: number[][]): boolean {
  // Maps prereq number to their followers
  let nodeMap: Map<number, Set<number>> = new Map();
  for (let i = 0; i < numCourses; i++) {
    nodeMap.set(i, new Set());
  }
  for (let [course, prereq] of prerequisites) {
    nodeMap.get(prereq)!.add(course);
  }

  let permMarks = new Set<number>();
  let tempMarks = new Set<number>();

  // Returns false if there's a cycle
  function visit(n: number): boolean {
    if (permMarks.has(n)) return true;
    if (tempMarks.has(n)) return false;

    tempMarks.add(n);

    for (let m of nodeMap.get(n)!) {
      if (!visit(m)) return false;
    }
    permMarks.add(n);

    return true;
  }

  for (let i = 0; i < numCourses; i++) {
    if (permMarks.has(i)) continue;
    if (!visit(i)) return false;
  }

  return true;
}
