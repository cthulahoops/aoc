from math import cos, sin, pi
from collections import namedtuple, defaultdict, Counter
import pytest
import numpy
import aoc

Transform = namedtuple('Transform', ('offset', 'rotation'))

def rotate_x(theta):
    return numpy.array([[1,0,0],[0,cos(theta),-sin(theta)],[0,sin(theta),cos(theta)]], dtype=int)

def rotate_y(theta):
    return numpy.array([[cos(theta),0,sin(theta)],[0,1,0],[-sin(theta),0,cos(theta)]], dtype=int)

def rotate_z(theta):
    return numpy.array([[cos(theta),-sin(theta),0],[sin(theta),cos(theta),0],[0,0,1]], dtype=int)

def generate_rotations():
    seen = set()
    rotations = []
    for x in [0, pi/2, pi, 3*pi/2]:
        for y in [0, pi/2, pi, 3*pi/2]:
            for z in [0, pi/2, pi, 3*pi/2]:
                rot = rotate_x(x) @ rotate_y(y) @ rotate_z(z)
                trot = tuple((tuple(x) for x in rot))
                if trot not in seen:
                    rotations.append(rot)
                    seen.add(trot)
    return rotations

ROTATIONS = generate_rotations()

class Point(tuple):
    @classmethod
    def from_string(cls, string):
        return cls(map(int, string.split(',')))

    def __add__(self, other):
        assert len(self) == len(other)
        return Point(((a + b) for a, b in zip(self, other)))

    def __sub__(self, other):
        assert len(self) == len(other)
        return Point(((a - b) for a, b in zip(self, other)))

    def rotate(self, matrix):
        return Point(numpy.array(self) @ matrix)

def parse_scanners(text):
    blocks = text.split('\n\n')
    return [[Point.from_string(point) for point in block.splitlines()[1:]] for block in blocks]


def find_matching(scanner_a, scanner_b, required_overlap=12):
    offsets = Counter(
        a - b
            for a in scanner_a
            for b in scanner_b
    )

    for offset in offsets:
        if offsets[offset] == required_overlap:
            return offset
    return None

def find_rotated_match(scanner_a, scanner_b, required_overlap=12):
    for rotation in ROTATIONS:
        rotated = [x.rotate(rotation) for x in scanner_b]
        offset = find_matching(scanner_a, rotated, required_overlap)
        if offset:
            return Transform(offset=offset, rotation=rotation)
    return None


def find_paths(graph, start):
    visited = {start: [start]}
    queue = [start]
    while queue:
        current = queue.pop()

        for next_node in graph[current]:
            if next_node in visited:
                continue
            visited[next_node] = visited[current] + [next_node]
            queue.append(next_node)
    return visited

def manhatten_distance(xs, ys):
    return sum(abs(x - y) for x, y in zip(xs, ys))

def test_manhatten_distance():
    assert manhatten_distance(Point((1, 7, 3)), Point((10, 1, 8))) == 9 + 6 + 5

def main():
    blocks = aoc.blocks(19)

    scanner_reports = [[Point.from_string(point) for point in block.splitlines()[1:]] for block in blocks]

    graph = defaultdict(list)
    transforms = dict()

    for (i, scanner_i) in enumerate(scanner_reports):
        for (j, scanner_j) in enumerate(scanner_reports):
            if j == i:
                continue
            trans = find_rotated_match(scanner_i, scanner_j)
            if trans:
                graph[j].append(i)
                transforms[(j, i)] = trans
                print(i, j, trans)

    paths = find_paths(graph, 0)
    print(paths)

    beacons = set()
    scanners = set()

    for (i, scanner_report) in enumerate(scanner_reports):
        scanner = Point((0, 0, 0))
        path = paths[i]
        path = path[::-1]
        path = list(zip(path, path[1:]))
        print(i, path)

        for i, j in path:
            trans = transforms[i, j]
            scanner_report = [beacon.rotate(trans.rotation) + trans.offset for beacon in scanner_report]
            scanner = scanner.rotate(trans.rotation) + trans.offset

        scanners.add(scanner)

        for beacon in scanner_report:
            beacons.add(beacon)

    print("Part 1: ", len(beacons))
    print("Part 2: ", max(manhatten_distance(x, y) for x in scanners for y in scanners))

if __name__ == '__main__':
    main()

simple = """--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1
"""

def test_simple():
    (scanner_a, scanner_b) = parse_scanners(simple)
    find_matching(scanner_a, scanner_b, required_overlap=3)

    assert find_matching(scanner_a, scanner_b, required_overlap=3) == Point((5, 2))
    assert find_matching(scanner_a, scanner_a, required_overlap=3) == Point((0, 0))

def test_rotations_matrices():
    assert len(generate_rotations()) == 24

rotations_test_data = """--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7

--- scanner 0 ---
1,-1,1
2,-2,2
3,-3,3
2,-1,3
-5,4,-6
-8,-7,0

--- scanner 0 ---
-1,-1,-1
-2,-2,-2
-3,-3,-3
-1,-3,-2
4,6,5
-7,0,8

--- scanner 0 ---
1,1,-1
2,2,-2
3,3,-3
1,3,-2
-4,-6,5
7,0,8

--- scanner 0 ---
1,1,1
2,2,2
3,3,3
3,1,2
-6,-4,-5
0,7,-8
"""
def test_rotations():
    test_data = parse_scanners(rotations_test_data)

    for x in test_data:
        for y in test_data:
            transform = find_rotated_match(x, y, 6)
            assert transform.offset == Point((0,0,0))

def test_find_paths():
    paths = find_paths({0: [1], 1: [0]}, 0)
    print(paths)
    assert paths[0] == [0]
    assert paths[1] == [0, 1]
