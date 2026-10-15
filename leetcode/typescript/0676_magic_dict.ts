class Trie {
  isLeaf: boolean;
  values: Map<string, Trie>;

  constructor() {
    this.values = new Map();
    this.isLeaf = false;
  }

  insert(value: string): void {
    if (value.length == 0) {
      this.isLeaf = true;
      return;
    }
    if (!this.values.has(value[0])) {
      this.values.set(value[0], new Trie());
    }
    this.values.get(value[0])!.insert(value.slice(1));
  }

  search(value: string, changed: boolean): boolean {
    if (value.length == 0) return changed && this.isLeaf;

    const c = value[0];
    const subValue = value.slice(1);

    // If changed: Only check for an exact match
    // Otherwise, check 'em all (and don't set `changed` for the current char)
    if (changed) {
      let sub = this.values.get(c);
      if (sub) return sub.search(subValue, true);
    } else {
      for (let k of this.values.keys()) {
        if (k == c) {
          if (this.values.get(k)!.search(subValue, false)) return true;
        } else {
          if (this.values.get(k)!.search(subValue, true)) return true;
        }
      }
    }
    return false;
  }
}

class MagicDictionary {
  trie: Trie;

  constructor() {
    this.trie = new Trie();
  }

  buildDict(dictionary: string[]): void {
    for (let entry of dictionary) {
      this.trie.insert(entry);
    }
  }

  search(searchWord: string): boolean {
    return this.trie.search(searchWord, false);
  }
}
