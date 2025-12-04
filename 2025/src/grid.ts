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

export class Grid {
  private map: Map<string, string>;

  constructor() {
    this.map = new Map();
  }

  set(location: Point, value: string) {
    this.map.set(location.toPair(), value);
  }

  get(location: Point) {
    return this.map.get(location.toPair());
  }

  *[Symbol.iterator](): Generator<[Point, string]> {
    for (const [pair, value] of this.map) {
      yield [Point.fromPair(pair), value];
    }
  }

  static parse(string: string): Grid {
    const lines = parseLines(string);
    const grid = new Grid();
    lines.forEach((line: string, y: number) => {
      line.split("").forEach((item: string, x: number) => {
        grid.set(new Point(x, y), item);
      });
    });
    return grid;
  }
}
