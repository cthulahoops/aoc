import math
from pansi import ansi
import aoc

def neighbours(point):
    (i, j) = point
    return [((i - 1), j), ((i + 1), j), (i, (j - 1)), (i, (j + 1))]

def display_grid(grid, lows, basins):
    for y in range(max(y for (_, y) in grid)):
        for x in range(max(x for (x, _) in grid)):
            height = grid[(x, y)]
            if (x, y) in lows:
                color = ansi.green
            elif (x, y) in basins[-1]:
                color = ansi.yellow
            elif (x, y) in basins[-2]:
                color = ansi.rgb['#aaa']
            elif (x, y) in basins[-3]:
                color = ansi.rgb['#9B4804']
            elif height == 9:
                color = ansi.red
            else:
                color = ''
            print(f"{color}{height}{ansi._}", end="")
        print()

def find_lows(grid):
    return {
        point
        for (point, value) in grid.items()
        if all(value < grid[neighbour] for neighbour in neighbours(point) if neighbour in grid)
    }

def flood_fill(grid, low):
    members = set()
    queue = [low]
    while queue:
        item = queue.pop()
        for neighbour in neighbours(item):
            if neighbour not in members and neighbour in grid and grid[neighbour] < 9:
                queue.append(neighbour)
                members.add(neighbour)
    return members

def main():
    grid = {
        (x, y): int(height)
        for (y, line) in enumerate(aoc.lines(9))
        for (x, height) in enumerate(line[:-1])
    }

    lows = find_lows(grid)

    part1 = sum(grid[point] + 1 for point in lows)

    basins = [flood_fill(grid, low) for low in lows]
    basins.sort(key=len)

    display_grid(grid, lows, basins)

    print("Part 1: ", part1)
    print("Part 2: ", math.prod(len(b) for b in basins[-3:]))

if __name__ == '__main__':
    main()
