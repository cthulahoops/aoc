from functools import lru_cache
import aoc

DOORWAYS = {(3,1), (5,1), (7,1), (9,1)}
# DESTINATIONS = {'A': {(3, 2), (3, 3)}, 'B': {(5, 2), (5,3)}, 'C': {(7,2),(7,3)}, 'D': {(9,2),(9,3)}}
DESTINATIONS = {'A': {(3, 2), (3, 3), (3, 4), (3, 5)}, 'B': {(5, 2), (5,3), (5,4), (5,5)}, 'C': {(7,2),(7,3),(7,4),(7,5)}, 'D': {(9,2),(9,3),(9,4),(9,5)}}
COSTS = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}

ROOMS = {v for (k, vs) in DESTINATIONS.items() for v in vs}

def neighbours(point):
    (x, y) = point
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

def parse_grid(lines, example=False):
    return {
        (x, y): value
        for (y, line) in enumerate(lines)
        for (x, value) in enumerate(line)
    }

def connected_empty(grid, point):
    result = set()
    queue = [point]
    while queue:
        item = queue.pop()
        result.add(item)
        for neighbour in neighbours(item):
            if neighbour not in result and grid[neighbour] == '.':
                queue.append(neighbour)
    result.remove(point)
    return result

def distance(point1, point2):
    (x1, y1) = point1
    (x2, y2) = point2
    if x1 == x2:
        return abs(y1 - y2)
    return abs(x1 - x2) + abs(y1 - 1) + abs(y2 - 1)

def at_home(amphipod, point):
    return point in DESTINATIONS[amphipod]

def room_clear(grid, amphipod):
    return all(grid[point] in ('.', amphipod) for point in DESTINATIONS[amphipod])

def blocking(grid, source):
    x, y = source
    blocked = [grid[x, y + dy] for dy in range(1, 4) if y + dy < 7]
    return any(b in 'ABCD' and grid[source] != b for b in blocked)

def valid_moves_from(grid, source):
    amphipod = grid[source]
    if amphipod not in 'ABCD':
        return

    for destination in connected_empty(grid, source):
        if destination in DOORWAYS:
            continue
        if destination in ROOMS and not (at_home(amphipod, destination) and room_clear(grid, amphipod)):
            continue
        if source not in ROOMS and not at_home(amphipod, destination):
            continue
        if source in ROOMS and at_home(amphipod, source) and not blocking(grid, source):
            continue

        yield destination

class Grid:
    def __init__(self, grid, pods):
        self.grid = grid
        self.pods = pods

    @classmethod
    def from_lines(cls, lines):
        grid = parse_grid(lines)
        pods = []
        for (point, value) in grid.items():
            if value in 'ABCD':
                pods.append((point, value))
                grid[point] = '.'
        pods = tuple(pods)
        return cls(grid, pods)

    def __getitem__(self, point):
        for (x, amphipod) in self.pods:
            if x == point:
                return amphipod
        return self.grid[point]

    def __hash__(self):
        return hash(self.pods)

    def __eq__(self, other):
        return self.pods == other.pods

    def move(self, source, destination):
        new_pods = tuple((destination, amphipod) if point == source else (point, amphipod) for point, amphipod in self.pods)
        return Grid(self.grid, new_pods)

    def display(self):
        for y in range(max(y + 1 for (x, y) in self.grid)):
            for x in range(13):
                try:
                    print(self[x, y], end='')
                except KeyError:
                    pass
            print()
        print()


def is_complete(grid):
    return all(
        grid[point] == amphipod
        for amphipod, points in DESTINATIONS.items()
        for point in points
    )

@lru_cache(maxsize=None)
def cost(grid):
    if is_complete(grid):
        return (0, [])

    best = (1000000000000000000000, None)

    for (source, amphipod) in grid.pods:
        for destination in valid_moves_from(grid, source):
            new_grid = grid.move(source, destination)

            rest_cost, rest_moves = cost(new_grid)

            move_cost = COSTS[amphipod] * distance(source, destination) + rest_cost
            if move_cost < best[0]:
                best = (move_cost, [(source, destination)] + rest_moves)

    return best


def main():
    lines = aoc.lines(23)
    # grid = Grid.from_lines(lines)

    # grid.display()

    # print(grid[(3, 2)])

    # best_cost, moves = cost(grid)
    # print("Part 1: ", best_cost)


    # grid.display()
    # for source, destination in moves:
    #     grid = grid.move(source, destination)
    # #    print("Move cost: ", distance(source, destination))
    #     grid.display()

    new_lines = ["  #D#C#B#A#","  #D#B#A#C#"]
    lines = lines[:3] + new_lines + lines[3:]
    grid = Grid.from_lines(lines)

    grid.display()

    for line in lines:
        print(line)

    best_cost, moves = cost(grid)
    print("Part 2: ", best_cost)

if __name__ == '__main__':
    main()

complete_grid = """#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########"""
def test_completion():
    lines = complete_grid.splitlines()
    grid = Grid.from_lines(lines)
    assert is_complete(grid)

