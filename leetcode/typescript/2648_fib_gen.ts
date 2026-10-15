function* fibGenerator(): Generator<number, any, number> {
  yield 0;
  yield 1;

  let a = 0;
  let b = 1;

  while (true) {
    const next = a + b;
    yield next;
    a = b;
    b = next;
  }
}
