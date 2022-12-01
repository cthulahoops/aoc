import aoc

def display_grid(grid):
    for y in range(max(y for (_, y) in grid) + 1):
        for x in range(max(x for (x, _) in grid) + 1):
            print(grid[x, y], end='')
        print()
    print()

def move_herd(grid, herd, direction):
    moves = set()
    for (point, cucumber) in grid.items():
        if cucumber == herd and grid[direction(point)] == '.':
            moves.add(point)

    for move in moves:
        grid[move] = '.'
        grid[direction(move)] = herd

    return len(moves)

def step(grid, east, south):
    c = 0
    c += move_herd(grid, '>', east)
    c += move_herd(grid, 'v', south)
    return c

def main():
    grid = aoc.grid(25)

    max_x = max(x for (x, _) in grid) + 1
    max_y = max(y for (_, y) in grid) + 1

    def east(point):
        x, y = point
        return ((x + 1) % max_x, y)

    def south(point):
        x, y = point
        return (x, (y + 1) % max_y)

    display_grid(grid)
    i = 0
    while i < 2000:
        i += 1
        moves = step(grid, east, south)
        print("Step ", i)
        print("Moves ", moves)
        # display_grid(grid)
        if moves == 0:
            break
    print("Part 1: ", i)


if __name__ == '__main__':
    main()
