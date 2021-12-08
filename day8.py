from collections import defaultdict
import aoc


DIGITS = [
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
]

digit_map = {k: str(v) for (v, k) in enumerate(DIGITS)}

def digits_to_int(digits):
    return int(''.join(digits))


def compute_signatures(digits):
    signatures = {}
    for wire in "abcdefg":
        signature = [0] * 8
        for digit in digits:
            if wire in digit:
                signature[len(digit)] += 1
        signatures[wire] = ''.join(str(s) for s in signature)
    return signatures

def main():
    signatures = {v: k for k, v in compute_signatures(DIGITS).items()}

    print(signatures)

    day1 = 0
    day2 = 0

    for line in aoc.lines(8):
        examples, code = line.split(' | ')
        examples = examples.split()
        code = code.split()

        for x in code:
            if len(x) in [len(DIGITS[1]), len(DIGITS[4]), len(DIGITS[7]), len(DIGITS[8])]:
                day1 += 1

        mapping = {ord(k): signatures[s] for (k, s) in compute_signatures(examples).items()}
        decoded = [''.join(sorted(x.translate(mapping))) for x in code]
        result = digits_to_int(digit_map[x] for x in decoded)
        day2 += result

    print("Day 1: ", day1)
    print("Day 2: ", day2)

if __name__ == '__main__':
    main()
