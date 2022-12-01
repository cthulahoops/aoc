import aoc

def part1():
    depth = 0
    position = 0

    for (direction, distance) in instructions:
        distance = int(distance)

        match direction:
            case "down":
                depth += distance
            case "up":
                depth -= distance
            case "forward":
                position += distance

    return depth * position

def part2():
    aim = 0
    depth = 0
    position = 0

    for (direction, distance) in instructions:
        distance = int(distance)

        match direction:
            case "down":
                aim += distance
            case "up":
                aim -= distance
            case "forward":
                position += distance
                depth += aim * distance

    return depth * position

instructions = aoc.words(2)
print("Part 1:", part1())
print("Part 2:", part2())
