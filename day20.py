from collections import defaultdict
import pytest
import aoc

def display_image(image):
    for y in range(min(y for (_, y) in image), max(y for (_, y) in image) + 1):
        for x in range(min(x for (x, _) in image), max(x for (x, _) in image) + 1):
            value = '#' if (x, y) in image else '.'
            print(f"{value}", end="")
        print()

def parse_image(image):
    return {
        (x, y)
        for (y, line) in enumerate(image.splitlines())
        for (x, value) in enumerate(line)
        if value == '#'
    }

def get_neighbours(pixel):
    (x, y) = pixel
    for dy in [-1, 0, 1]:
        for dx in [-1, 0, 1]:
            yield (x + dx, y + dy)

def compute_offsets(image):
    offsets = defaultdict(int)
    for pixel in image:
        for (i, neighbour) in enumerate(get_neighbours(pixel)):
            offsets[neighbour] += 1 << i
    return offsets

def apply_algorithm(image, algorithm):
    offsets = defaultdict(int)
    for pixel in image:
        for (i, neighbour) in enumerate(get_neighbours(pixel)):
            offsets[neighbour] += 1 << i

    new_image = set()
    for (k, v) in offsets.items():
        if algorithm[v] == '#':
            new_image.add(k)
    return new_image


def main():
    (algorithm, image) = aoc.blocks(20)
    assert len(algorithm) == 512
    image = parse_image(image)

    display_image(image)
    print(len(image))

    for _ in range(2):
        image = apply_algorithm(image, algorithm)
        display_image(image)
        print(len(image))

if __name__ == '__main__':
    main()


example_image = """#..#.
#....
##..#
..#..
..###
"""
def test_grid_offset():
    image = parse_image(example_image)
    offsets = compute_offsets(image)
    assert offsets[(2,2)] == 34

dark_algorithm = "#.#..#.#...##.###.##.#.###....#..##..#.#####.#.##.#.##..##..#.#.######..##..####.#....#.#....##....#####..#######..###..#.##..#....#....#....#..#..#..##...####..###.##..##..#.#.#.#.#.#.#..##.###.##.#.##.#.##.#####..###.#.#...##..##...###...###..##...##.######....####.###...####.##.....###.##.#.##.#.....##.##..###.....#..##....#.##.#...##.###.###.#..####....#.###...#....#..###...##..#####..#.######..#.#....####.####.#.#....#.###..##...#.###.####....#.##.#....##...##.#..#....#.##...#....#.####..#.#..####.#..."
dark_example = "#"
def test_dark_example():
    image =
