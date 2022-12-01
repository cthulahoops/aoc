from dataclasses import dataclass
import aoc
from aoc import Point

@dataclass
class Fold:
    axis: str
    value: int

    @classmethod
    def from_string(cls, fold):
        (var, value) = fold.split('=')
        return cls(var[-1], int(value))

    def apply(self, point):
        match self.axis:
            case 'x':
                if point.x > self.value:
                    return Point(2 * self.value - point.x, point.y)
                else:
                    return point
            case 'y':
                if point.y > self.value:
                    return Point(point.x, 2 * self.value - point.y)
                else:
                    return point

def view_paper(points):
    for y in range(max(points, key=lambda p: p.y).y + 1):
        for x in range(max(points, key=lambda p: p.x).x + 1):
            if Point(x, y) in points:
                print('#', end='')
            else:
                print('-', end='')
        print()

def main():
    points, folds = aoc.blocks(13)
    points = [Point.from_string(line) for line in points.splitlines()]
    folds = [Fold.from_string(fold) for fold in folds.splitlines()]

    print("Part 1:", len(set(folds[0].apply(p) for p in points)))

    for fold in folds:
        points = set(fold.apply(p) for p in points)

    print("Part 2:")
    view_paper(points)

if __name__ == '__main__':
    main()
