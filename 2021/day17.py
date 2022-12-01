from dataclasses import dataclass
import re
import aoc
from aoc import Point

@dataclass
class Region:
    x0: int
    x1: int
    y0: int
    y1: int

    @classmethod
    def parse_string(cls, string):
        match = re.match(r"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)", string)
        return cls(*map(int, match.groups()))

    def in_region(self, point):
        return self.x0 <= point.x <= self.x1 and self.y0 <= point.y <= self.y1

@dataclass
class Probe:
    position: Point
    velocity: Point

    def update(self):
        self.position += self.velocity
        if self.velocity.x > 0:
            self.velocity += Point(-1, 0)
        elif self.velocity.x < 0:
            self.velocity += Point(1, 0)
        self.velocity += Point(0, -1)

def peak_height(velocity, target):
    probe = Probe(position=Point(0, 0), velocity=velocity)

    peak = probe.position.y
    while probe.position.y >= target.y0 and probe.position.x <= target.x1:
        if probe.velocity.x == 0 and probe.position.x < target.x0:
            break
        if target.in_region(probe.position):
            return peak
        probe.update()
        if probe.position.y > peak:
            peak = probe.position.y

def main():
    target = Region.parse_string(aoc.lines(17)[0])
    print(target)

    hits = 0
    max_peak = 0
    for dx in range(1, target.x1 + 1):
        for dy in range(target.y0, -target.y0):
            peak = peak_height(Point(dx, dy), target)
            if peak is None:
                continue
            if peak > max_peak:
                max_peak = peak
            hits += 1

    print("Part 1: ", max_peak)
    print("Part 2: ", hits)

if __name__ == '__main__':
    main()
