from collections import namedtuple
from dataclasses import dataclass
import aoc

@dataclass
class Range:
    low: int
    high: int

    def __contains__(self, point):
        return self.low <= point <= self.high

    def __len__(self):
        return max(self.high - self.low + 1, 0)

    def __and__(self, other):
        return Range(max(self.low, other.low), min(self.high, other.high))

@dataclass
class Cuboid:
    x_range: Range
    y_range: Range
    z_range: Range

    def __contains__(self, point):
        (x, y, z) = point
        return x in self.x_range and y in self.y_range and z in self.z_range

    def __and__(self, other):
        return len(self.x_range & other.x_range) > 0 and len(self.y_range & other.y_range) > 0 and len(self.z_range & other.z_range) > 0

    def volume(self):
        return len(self.x_range) * len(self.y_range) * len(self.z_range)


def parse_state(string):
    match string:
        case "on":
            return True
        case "off":
            return False
        case _:
            raise ValueError(string)

def parse_range(string):
    _axis, string = string.split('=')
    low, high = string.split('..')
    return Range(int(low), int(high))

def parse_step(line):
    (state, ranges) = line.split()
    x, y, z = [parse_range(string) for string in ranges.split(',')]
    return parse_state(state), Cuboid(x, y, z)

def sampling_count(steps):
    x_steps = set()
    y_steps = set()
    z_steps = set()
    for step in steps:
        x_steps.add(step.cuboid.x_range.low)
        x_steps.add(step.cuboid.x_range.high + 1)
        y_steps.add(step.cuboid.y_range.low)
        y_steps.add(step.cuboid.y_range.high + 1)
        z_steps.add(step.cuboid.z_range.low)
        z_steps.add(step.cuboid.z_range.high + 1)
    x_steps = sorted(x_steps)
    y_steps = sorted(y_steps)
    z_steps = sorted(z_steps)

    count = 0

    steps = steps[::-1]

    for (x, x1) in zip(x_steps, x_steps[1:]):
        for (y, y1) in zip(y_steps, y_steps[1:]):
            for (z, z1) in zip(z_steps, z_steps[1:]):
                for step in steps:
                    if (x, y, z) in step.cuboid:
                        if step.state:
                            count += (x1 - x) * (y1 - y) * (z1 - z)
                        break

    return count

def split_cuboid(cuboid, point):
    def low(range, split_point):
        return Range(range.low, min(split_point - 1, range.high))
    def high(range, split_point):
        return Range(max(split_point, range.low), range.high)

    (x, y, z) = point
    cubes = [
        Cuboid(x_range=xs(cuboid.x_range, x), y_range=ys(cuboid.y_range, y), z_range=zs(cuboid.z_range, z))
        for xs in [low, high]
        for ys in [low, high]
        for zs in [low, high]
    ]
    return [c for c in cubes if c.volume() > 0]

def split_step(step):
    cuboids = split_cuboid(step.cuboid, (1000, 1000, 1000))
    return [Step(index=step.index, state=step.state, cuboid=c) for c in cuboids]


def split_non_intersecting_groups(steps):
    groups = []
    for step in steps:
        intersects = [g for g in groups if any(x.cuboid & step.cuboid for x in g)]
        if intersects:
            intersects[0].append(step)
            for i in intersects[1:]:
                intersects[0].extend(i)
                i[:] = []
        else:
            groups.append([step])

    return [sorted(g, key=lambda x: x.index) for g in groups if g]

Step = namedtuple('Step', ('index', 'state', 'cuboid'))

def main():
    steps = [parse_step(x) for x in aoc.lines(22)]
    steps = [Step(index=i, state=state, cuboid=cuboid) for (i, (state, cuboid)) in enumerate(steps)]

    print("Step count: ", len(steps))
#    print("Slow: ", sampling_count(steps))

    split = []
    for x in steps:
        split.extend(split_step(x))
    steps = split

    groups = split_non_intersecting_groups(steps)

    assert sum(len(g) for g in groups) == len(steps), sum(len(g) for g in groups)
    print("Groups: ", len(groups), [len(g) for g in groups], sum(len(g) for g in groups))

    total = 0
    for idx, group in enumerate(groups):
        print("Group: ", idx, len(group))
#        print(group)
        count = sampling_count(group)
        print("Count: ", count)
        if count == 0:
            print(group)
        total += count
    print("Part 2: ", total)
  #  print("Count: ", count)

if __name__ == '__main__':
    main()

def test_sampling():
    steps = [
        Step(index=0, state=True, cuboid=Cuboid(Range(0,4), Range(0,4), Range(0,4))),
        Step(index=1, state=True, cuboid=Cuboid(Range(7,7), Range(7,7), Range(7,7))),
    ]
    assert sampling_count(steps) == 126

    steps = [
        Step(index=0, state=True, cuboid=Cuboid(Range(0,4), Range(0,4), Range(0,4))),
        Step(index=1, state=False, cuboid=Cuboid(Range(0,4), Range(0,4), Range(0,4))),
    ]
    assert sampling_count(steps) == 0

def test_parse_step():
    step = parse_step("off x=-48..-32,y=-32..-16,z=-15..-5")
    assert (False, Cuboid(Range(-48, -32), Range(-32, -16), Range(-15, -5))) == step

def test_cuboid():
    cuboid = Cuboid(Range(1, 5), Range(10, 15), Range(-3, 3))
    assert (3, 13, 3) in cuboid

def test_range():
    x = Range(1, 5)
    y = Range(-2, 3)

    assert len(x) == 5
    assert len(y) == 6
    assert x & y == Range(1, 3)

def test_split():
    cuboid = Cuboid(Range(0, 10), Range(3, 6), Range(-9, -8))

    assert split_cuboid(cuboid, (50, 50, 50)) == [cuboid]

    sp = split_cuboid(cuboid, (5, 0, 0))
    assert len(sp) == 2
    assert sp[0].volume() + sp[1].volume() == cuboid.volume()
