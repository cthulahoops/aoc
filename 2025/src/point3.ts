export class Point3 {
  x: number;
  y: number;
  z: number;

  constructor(x: number, y: number, z: number) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  static fromString(pair: string) {
    const [x, y, z] = pair.split(",");
    return new Point3(Number(x), Number(y), Number(z));
  }

  toString() {
    return `${this.x},${this.y},${this.z}`;
  }

  sqDist(other: Point3) {
    return (
      Math.pow(this.x - other.x, 2) +
      Math.pow(this.y - other.y, 2) +
      Math.pow(this.z - other.z, 2)
    );
  }
}
