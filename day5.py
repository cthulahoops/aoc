from dataclasses import dataclass
from collections import Counter
import aoc

@dataclass(frozen=True)
class Point:
    x: int
    y: int

    @classmethod
    def from_string(cls, string):
        x, y = string.split(',')
        return cls(int(x), int(y))

@dataclass(frozen=True)
class LineSegment:
    start: Point
    end: Point

    @classmethod
    def from_string(cls, string):
        start, end = string.split(' -> ')
        return cls(Point.from_string(start), Point.from_string(end))

    def is_vertical(self):
        return self.start.x == self.end.x

    def is_horizontal(self):
        return self.start.y == self.end.y

    def points(self):
        segment_length = max(abs(self.end.x - self.start.x), abs(self.end.y - self.start.y))
        mx = (self.end.x - self.start.x) // segment_length
        my = (self.end.y - self.start.y) // segment_length
        return [Point(self.start.x + t * mx, self.start.y + t * my) for t in range(segment_length + 1)]

def count_overlaps(segments):
    counter = Counter()
    for segment in segments:
        counter.update(segment.points())
    return sum(1 for k, v in counter.items() if v > 1)

def main():
    segments = [LineSegment.from_string(line) for line in aoc.lines(5)]

    print("Part 1:", count_overlaps(s for s in segments if s.is_vertical() or s.is_horizontal()))
    print("Part 2:", count_overlaps(segments))

if __name__ == '__main__':
    main()
