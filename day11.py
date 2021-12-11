import time
from pansi import ansi
import aoc

def neighbours(point):
    (x, y) = point

    return [(x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y + 1), (x, y - 1), (x - 1, y + 1), (x - 1, y), (x - 1, y - 1)]

def display_grid(grid, flashes):
    for y in range(max(y for (_, y) in grid) + 1):
        for x in range(max(x for (x, _) in grid) + 1):
            energy = grid[(x, y)]
            r = g = b = energy + 6
            color = ansi.rgb[f'#{r:x}{g:x}{b:x}']
            if (x, y) in flashes:
                color = ansi.cyan
            print(f"{color}{energy}{ansi._}", end="")
        print()
    print()

def step(grid):
    flashes = []

    for point in grid:
        grid[point] += 1
        if grid[point] == 10:
            flashes.append(point)

    for point in flashes:
        for neighbour in neighbours(point):
            if neighbour not in grid:
                continue
            grid[neighbour] += 1
            if grid[neighbour] == 10:
                flashes.append(neighbour)

    assert len(flashes) == len(set(flashes))

    for point in flashes:
        grid[point] = 0

    return set(flashes)

def main():
    grid = aoc.int_grid(11)

    n = 0
    part1 = 0

    display_grid(grid, set())
    while True:
        n += 1
        print("\x1b[2J\x1b[H")
        print(f"Step: {n}")
        flashes = step(grid)
        display_grid(grid, flashes)
        if n <= 100:
            part1 += len(flashes)

        if len(flashes) == 100:
            break

        time.sleep(0.05)

    print("Part 1: ", part1)
    print("Part 2: ", n)

if __name__ == '__main__':
    main()
