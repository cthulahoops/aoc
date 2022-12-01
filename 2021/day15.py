import heapq
from pansi import ansi
import aoc

def display_grid(grid, path):
    for y in range(max(y for (_, y) in grid) + 1):
        for x in range(max(x for (x, _) in grid) + 1):
            if (x, y) in path:
                color = ansi.green
            else:
                color = ''
            cost = grid[(x, y)]
            print(f"{color}{cost}{ansi._}", end="")
        print()
    print()

def neighbors(location):
    x, y = location
    return [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

def solve_astar(grid):
    max_x = max(x for (x, y) in grid)
    max_y = max(y for (x, y) in grid)

    def cost_estimate(x, y):
        return (max_x - x) + (max_y - y)

    start = (0, 0)
    end = (max_x, max_y)

    queue = []
    heapq.heappush(queue, (cost_estimate(*start), start, 0, None))

    visited = dict()

    while queue:
        (cost, current, distance, source) = heapq.heappop(queue)
        if current in visited:
            continue

        visited[current] = source

        if current == end:
            break

        for next_node in neighbors(current):
            if next_node not in grid or next_node in visited:
                continue
            value = grid[next_node]

            heapq.heappush(queue, (distance + value + cost_estimate(*next_node), next_node, distance + value, current))

    cost = 0
    path = []
    current = end
    while current != start:
        path.append(current)
        cost += grid[current]
        current = visited[current]

    return cost, path

def rotate_risk(risk, steps):
    return ((risk - 1) + steps) % 9 + 1

assert rotate_risk(2, 2) == 4
assert rotate_risk(9, 1) == 1

def embiggen(grid):
    big_grid = dict()

    size_x = max(x for (x, y) in grid) + 1
    size_y = max(y for (x, y) in grid) + 1

    for (x, y) in grid:
        for i in range(5):
            for j in range(5):
                big_grid[(x + i * size_x, y + j * size_y)] = rotate_risk(grid[(x, y)], i + j)
    return big_grid

def main():
    grid = aoc.int_grid(15)

    cost, path = solve_astar(grid)
    display_grid(grid, path)
    print("Part 1: ", cost)

    cost, path = solve_astar(embiggen(grid))
    print("Part 2: ", cost)

if __name__ == '__main__':
    main()
