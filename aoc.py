from dataclasses import dataclass

def input_file(day, example=False):
    return f"input/day{day}{'-example' if example else ''}"

def words(day):
    return [line.split() for line in lines(day)]

def ints(day):
    return [int(line) for line in lines(day)]

def comma_ints(day):
    return [int(x) for x in lines(day)[0].split(',')]

def lines(day, example=False):
    return open(input_file(day, example)).read().splitlines()

def blocks(day, example=False):
    return open(input_file(day, example)).read().split("\n\n")

def int_grid(day, example=False):
    return {
        (x, y): int(value)
        for (y, line) in enumerate(lines(day, example))
        for (x, value) in enumerate(line)
    }

@dataclass(frozen=True)
class Point:
    x: int
    y: int

    @classmethod
    def from_string(cls, string):
        x, y = string.split(',')
        return cls(int(x), int(y))
