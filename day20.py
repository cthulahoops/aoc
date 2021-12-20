import pytest
import aoc

class Image:
    def __init__(self, image, background, min_y, max_y, min_x, max_x):
        self.image = image
        self.background = background
        self.min_y = min_y
        self.max_y = max_y
        self.min_x = min_x
        self.max_x = max_x

    @classmethod
    def from_string(cls, image_data):
        lines = image_data.splitlines()
        image = {}
        for (y, line) in enumerate(lines):
            for (x, char) in enumerate(line):
                image[x, y] = char
        return cls(image, '.', 0, len(lines), 0, len(lines[0]))

    def enhance(self, algorithm):
        new_image = {}
        for y in range(self.min_y - 1, self.max_y + 1):
            for x in range(self.min_x - 1, self.max_x + 1):
                offset = 0
                for (i, neighbour) in enumerate(get_neighbours((x, y))):
                    if self.image.get(neighbour, self.background) == '#':
                        offset += 256 >> i
                new_image[(x, y)] = algorithm[offset]

        if self.background == '.':
            new_background = algorithm[0]
        else:
            new_background = algorithm[511]

        return Image(new_image, new_background, self.min_y - 1, self.max_y + 1, self.min_x - 1, self.max_y + 1)

    def display(self):
        for y in range(self.min_y - 2, self.max_y + 2):
            for x in range(self.min_x - 2, self.max_x + 2):
                print(self.image.get((x, y), self.background), end='')
            print()

    def lit_pixels(self):
        if self.background == '#':
            raise ValueError("Infinite!")

        return sum(1 for v in self.image.values() if v == '#')

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

def main():
    (algorithm, image) = aoc.blocks(20)
    assert len(algorithm) == 512
    image = Image.from_string(image)

    image.display()

    for _ in range(2):
        image = image.enhance(algorithm)
        image.display()

    print("Part 1: ", image.lit_pixels())

    for _ in range(48):
        image = image.enhance(algorithm)

    print("Part 2: ", image.lit_pixels())


if __name__ == '__main__':
    main()
