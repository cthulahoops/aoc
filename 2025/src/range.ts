export class Range {
  start: number;
  end: number;

  constructor(start: number, end: number) {
    this.start = start;
    this.end = end;
  }

  intersects(other: Range) {
    return (
      this.contains(other.start) ||
      this.contains(other.end) ||
      other.contains(this.start) ||
      other.contains(this.end)
    );
  }

  contains(value: number) {
    return value >= this.start && value <= this.end;
  }

  static parse(text: string) {
    const [start, end] = text.split("-");
    return new Range(Number(start), Number(end));
  }

  get length() {
    return this.end - this.start + 1;
  }

  merge(other: Range) {
    if (!this.intersects(other)) {
      throw new Error("Ranges do not overlap");
    }
    return new Range(
      Math.min(this.start, other.start),
      Math.max(this.end, other.end),
    );
  }
}
