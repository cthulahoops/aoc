import pytest
import aoc

def add_left(addition, snail):
    if addition is None:
        return snail

    match snail:
        case left, right:
            return [add_left(addition, left), right]
        case simple:
            return addition + simple

def add_right(addition, snail):
    if addition is None:
        return snail

    match snail:
        case left, right:
            return [left, add_right(addition, right)]
        case simple:
            return addition + simple


def explode_walker(snail, depth):
    match snail:
        case left, right:
            new_left, explosion = explode_walker(left, depth + 1)
            if explosion is not None:
                (explosion_left, explosion_right) = explosion
                return [new_left, add_left(explosion_right, right)], (explosion_left, None)

            new_right, explosion = explode_walker(right, depth + 1)

            if explosion is not None:
                (explosion_left, explosion_right) = explosion
                return [add_right(explosion_left, left), new_right], (None, explosion_right)

            if depth >= 4:
                return 0, [left, right]
            return [left, right], None
        case _:
            return (snail, None)

def explode(snail):
    (result, _) = explode_walker(snail, 0)
    return result

def split(snail):
    match snail:
        case left, right:
            new_left = split(left)
            if new_left == left:
                return [new_left, split(right)]
            return [new_left, right]

        case _:
            if snail >= 10:
                half = snail // 2
                return [half, snail - half]
            return snail

def add(a, b):
    snail = [a, b]

    while True:
        exploded_snail = explode(snail)
        if exploded_snail != snail:
            snail = exploded_snail
            continue
        split_snail = split(snail)
        if split_snail == snail:
            break
        snail = split_snail
    return snail


def sum(snails):
    total = snails[0]
    for x in snails[1:]:
        total = add(total, x)
    return total

def magnitude(snail):
    match snail:
        case left, right:
            return 3 * magnitude(left) + 2 * magnitude(right)
        case _:
            return snail

def main():
    lines = [eval(x) for x in aoc.lines(18)]
    print("Part 1: ", magnitude(sum(lines)))

    print("Part 2: ", max(magnitude(add(x, y)) for x in lines for y in lines if x != y))

if __name__ == '__main__':
    main()

explode_tests = [
    (7, 7),
    ([[[[0, 9], 2], 3], 4], [[[[0, 9], 2], 3], 4]),
    ([[[[[9, 8], 1], 2], 3], 4], [[[[0, 9], 2], 3], 4]),
    ([7, [6, [5, [4, [3, 2]]]]], [7, [6, [5, [7, 0]]]]),
    ([[6, [5, [4, [3, 2]]]], 1], [[6, [5, [7, 0]]], 3]),
    ([[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]], [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]),
    ([[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]], [[3, [2, [8, 0]]], [9, [5, [7, 0]]]]),
]

@pytest.mark.parametrize("number,result", explode_tests)
def test_explode(number, result):
    assert explode(number) == result

split_tests = [
    (7, 7),
    (13, [6, 7]),
    ([1, 15], [1, [7, 8]]),
    ([16, 12], [[8, 8], 12]),
]

@pytest.mark.parametrize("number,result", split_tests)
def test_split(number, result):
    assert split(number) == result


def test_add():
    assert add([[[[4, 3], 4], 4], [7, [[8, 4], 9]]],  [1, 1]) == [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]]

total_tests = [
    ([[1,1], [2,2], [3,3], [4,4]], [[[[1,1],[2,2]],[3,3]],[4,4]]),
    ([[1, 1], [2, 2], [3, 3], [4, 4], [5, 5]], [[[[3, 0], [5, 3]], [4, 4]], [5, 5]]),
    ([[1, 1], [2, 2], [3, 3], [4, 4], [5, 5], [6, 6]], [[[[5, 0], [7, 4]], [5, 5]], [6, 6]]),
]


@pytest.mark.parametrize("input_list,total", total_tests)
def test_sum(input_list, total):
    assert sum(input_list) == total
