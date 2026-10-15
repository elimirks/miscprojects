class MinStack {
  stack: number[];
  minStack: number[];

  constructor() {
    this.stack = [];
    this.minStack = [];
  }

  push(val: number): void {
    this.stack.push(val);
    if (this.minStack.length == 0) {
      this.minStack.push(val);
    } else {
      this.minStack.push(Math.min(val, this.getMin()));
    }
  }

  pop(): void {
    this.stack.pop();
    this.minStack.pop();
  }

  top(): number {
    if (this.stack.length == 0) throw "Empty stack";
    return this.stack[this.stack.length - 1];
  }

  getMin(): number {
    if (this.minStack.length == 0) throw "Empty stack";
    return this.minStack[this.minStack.length - 1];
  }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * var obj = new MinStack()
 * obj.push(val)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.getMin()
 */
