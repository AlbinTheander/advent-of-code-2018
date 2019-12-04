export default class Grid<T> {
  data: T[][];
  height: number;
  width: number;

  constructor(data: T[][]) {
    this.data = data;
    this.height = data.length;
    this.width = data[0].length;
  }

  cloneAndClear(value: T): Grid<T> {
    const data: T[][] = Array(this.height)
      .fill(0)
      .map(() => Array(this.width).fill(value));
    return new Grid(data);
  }

  validCoords(x: number, y: number): boolean {
    return y >= 0 && y < this.height && x >= 0 && x < this.width;
  }

  get(x: number, y: number): T | null {
    if (!this.validCoords(x, y)) return null;
    return this.data[y][x];
  }

  set(x: number, y: number, value: T): void {
    if (this.validCoords(x, y)) this.data[y][x] = value;
  }

  neighbors(x: number, y: number): T[] {
    const result = [];
    for (let dy = -1; dy <= 1; dy++)
      for (let dx = -1; dx <= 1; dx++)
        if (this.validCoords(x + dx, y + dy) && (dx !== 0 || dy !== 0))
          result.push(this.data[y + dy][x + dx]);
    return result;
  }

  toString(): string {
    return this.data.map(line => line.join('')).join('\n');
  }
}
