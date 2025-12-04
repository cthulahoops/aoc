import { parseLines } from "./parse";

export class Point {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  static fromPair(pair: string) {
    const [x, y] = pair.split(",");
    return new Point(Number(x), Number(y));
  }

  toPair() {
    return `${this.x},${this.y}`;
  }

  neighbours() {
    return [
      new Point(this.x + 1, this.y),
      new Point(this.x + 1, this.y + 1),
      new Point(this.x, this.y + 1),
      new Point(this.x - 1, this.y + 1),
      new Point(this.x - 1, this.y),
      new Point(this.x - 1, this.y - 1),
      new Point(this.x, this.y - 1),
      new Point(this.x + 1, this.y - 1),
    ];
  }
}

export class Grid<T> {
  private map: Map<string, T>;
  maxX: number = 0;
  maxY: number = 0;

  constructor() {
    this.map = new Map();
  }

  set(location: Point, value: T) {
    if (location.x > this.maxX) {
      this.maxX = location.x;
    }
    if (location.y > this.maxY) {
      this.maxY = location.y;
    }
    this.map.set(location.toPair(), value);
  }

  get(location: Point): T | undefined {
    return this.map.get(location.toPair());
  }

  delete(location: Point) {
    this.map.delete(location.toPair());
  }

  *[Symbol.iterator](): Generator<[Point, T]> {
    for (const [pair, value] of this.map) {
      yield [Point.fromPair(pair), value];
    }
  }

  static parse<T>(string: string, f: (item: string) => T | undefined): Grid<T> {
    const lines = parseLines(string);
    const grid = new Grid<T>();
    lines.forEach((line: string, y: number) => {
      line.split("").forEach((item: string, x: number) => {
        const value = f(item);
        if (value !== undefined) {
          grid.set(new Point(x, y), value);
        }
      });
    });
    return grid;
  }
}
