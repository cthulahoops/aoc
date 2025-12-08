interface Stringy {
  toString(): string;
}

interface SerializableStringy extends Stringy {
  toString(): string;
}

interface SerializableStringyConstructor<T extends SerializableStringy> {
  fromString(str: string): T;
}

export class SMap<K extends SerializableStringy, T> {
  private map = new Map<string, T>();
  private keyConstructor: SerializableStringyConstructor<K>;

  set(key: K, value: T): void {
    this.map.set(key.toString(), value);
  }
  constructor(keyConstructor: SerializableStringyConstructor<K>) {
    this.keyConstructor = keyConstructor;
  }

  get(key: K): T | undefined {
    return this.map.get(key.toString());
  }

  delete(key: K) {
    this.map.delete(key.toString());
  }

  *[Symbol.iterator](): Generator<[K, T]> {
    for (const [keyStr, value] of this.map) {
      const key = this.keyConstructor.fromString(keyStr);
      yield [key, value];
    }
  }
}
