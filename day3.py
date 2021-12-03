from collections import Counter
import aoc


def bit(value):
    return '1' if value else '0'

def count_bit(numbers, i):
    return Counter(n[i] for n in numbers)

def part1(numbers):
    gamma = []
    epsilon = []
    for i in range(12):
        count = count_bit(numbers, i)
        gamma.append(bit(count['1'] > count['0']))
        epsilon.append(bit(count['0'] > count['1']))

    return int(''.join(gamma), 2) * int(''.join(epsilon), 2)


def bit_criteria_search(criteria, numbers):
    selected = numbers
    for i in range(12):
        count = count_bit(selected, i)
        selected_bit = bit(criteria(count['1'], count['0']))

        selected = [x for x in selected if x[i] == selected_bit]

        if len(selected) == 1:
            return int(selected[0], 2)

def main():
    lines = aoc.lines(3)

    print("Part 1: ", part1(lines))

    oxygen_rating = bit_criteria_search(lambda ones, zeros: ones >= zeros, lines)
    co2_rating = bit_criteria_search(lambda ones, zeros: zeros > ones, lines)

    print(oxygen_rating, co2_rating)

    print("Part 2: ", oxygen_rating * co2_rating)

if __name__ == '__main__':
    main()
