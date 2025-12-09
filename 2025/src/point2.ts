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
    const width = 1 + Math.abs(this.x - other.x);
    const height = 1 + Math.abs(this.y - other.y);
    return width * height;
  }

  add(other: Point2) {
    return new Point2(this.x + other.x, this.y + other.y);
  }

  sub(other: Point2) {
    return new Point2(this.x - other.x, this.y - other.y);
  }

  sign() {
    return new Point2(Math.sign(this.x), Math.sign(this.y));
  }

  equals(other: Point2) {
    return this.x == other.x && this.y == other.y;
  }

  div(other: number): Point2 {
    return new Point2(this.x / other, this.y / other);
  }
}
