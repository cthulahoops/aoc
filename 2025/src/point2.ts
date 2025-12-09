export class Point2 {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  static fromString(pair: string) {
    const [x, y] = pair.split(",");
    return new Point2(Number(x), Number(y));
  }

  toString() {
    return `${this.x},${this.y}`;
  }

  sqDist(other: Point2) {
    return Math.pow(this.x - other.x, 2) + Math.pow(this.y - other.y, 2);
  }

  area(other: Point2) {
    const width = 1 + this.x - other.x;
    const height = 1 + this.y - other.y;
    return width * height;
  }
}
