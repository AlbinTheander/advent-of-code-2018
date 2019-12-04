type ToString<T> = (t: T) => string;

export class BetterSet<Value> {
  items = new Map<string, Value>();
  valueFn: ToString<Value>;

  constructor(valueFn: ToString<Value>) {
    this.valueFn = valueFn;
  }

  add(v: Value): BetterSet<Value> {
    this.items.set(this.valueFn(v), v);
    return this;
  }

  has(v: Value): boolean {
    return this.items.has(this.valueFn(v));
  }

  values(): IterableIterator<Value> {
    return this.items.values();
  }
}
