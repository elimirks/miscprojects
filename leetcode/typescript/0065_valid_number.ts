enum Sign {
  Plus,
  Minus,
}

class Parser {
  pos: number;
  tokens: string;

  constructor(s: string) {
    this.pos = 0;
    this.tokens = s;
  }

  digit(): number | null {
    if (this.pos >= this.tokens.length) return null;
    let code = this.tokens.charCodeAt(this.pos);
    let num = code - 48;
    if (num >= 0 && num <= 9) {
      this.pos++;
      return num;
    }
    return null;
  }

  unsignedInt(): number | null {
    let num = 0;
    let count = 0;

    let d: number | null = null;
    do {
      d = this.digit();
      if (d !== null) {
        count++;
        num *= 10;
        num += d;
      }
    } while (d !== null);

    return (count > 0) ? num : null;
  }

  signedInt(): number | null {
    const initialPos = this.pos;
    const sign = this.sign();
    const int = this.unsignedInt();

    if (int === null) {
      this.pos = initialPos;
      return null;
    }
    return (sign === Sign.Minus) ? -int : int;
  }

  /// Returns true if a dot was parsed
  dot(): boolean {
    if (this.tokens[this.pos] == ".") {
      this.pos++;
      return true;
    }
    return false;
  }

  sign(): Sign | null {
    const c = this.tokens[this.pos];
    if (c == "+") {
      this.pos++;
      return Sign.Plus;
    } else if (c == "-") {
      this.pos++;
      return Sign.Minus;
    }
    return null;
  }

  signedFloat(): number | null {
    const initialPos = this.pos;

    const sign = this.sign();
    const signMultiplier = (sign === Sign.Minus) ? -1 : 1;

    const firstNum = this.unsignedInt();
    const hasDot = this.dot();

    if (hasDot) {
      const secondNum = this.unsignedInt();

      if (secondNum !== null) {
        if (firstNum === null) {
          return signMultiplier * asDecimal(secondNum);
        }
        return signMultiplier * firstNum + asDecimal(secondNum);
      }
    }
    if (firstNum === null) {
      this.pos = initialPos;
      return null;
    }
    return signMultiplier * firstNum;
  }

  // Returns true if e was parsed
  e(): boolean {
    if (this.pos >= this.tokens.length) return false;
    let c = this.tokens[this.pos];
    if (c.toLowerCase() == "e") {
      this.pos++;
      return true;
    }
    return false;
  }

  fullNum(): number | null {
    const initialPos = this.pos;
    let firstNum = this.signedFloat();
    if (firstNum === null)  {
      this.pos = initialPos;
      return null;
    }

    let hasExponent = this.e();
    if (!hasExponent) {
      return firstNum;
    }

    let secondNum = this.signedInt();
    if (secondNum === null) {
      this.pos = initialPos;
      return null;
    }

    return firstNum + Math.pow(10, secondNum);
  }

  atEnd(): boolean {
    return this.pos == this.tokens.length;
  }
}

function asDecimal(n: number): number {
  let divisor = 10;
  while (n >= divisor) {
    divisor *= 10;
  }
  return n / divisor;
}

function isNumber(s: string): boolean {
  let parser = new Parser(s);
  if (parser.fullNum() === null) return false;
  return parser.atEnd();
}
