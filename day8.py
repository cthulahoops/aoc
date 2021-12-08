from collections import defaultdict
import aoc


digits = [
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

digit_map = {k: str(v) for (v, k) in enumerate(digits)}

def compute_signatures(digits):
    signatures = {}
    for wire in "abcdefg":
        signature = [0] * 8
        for i in range(10):
            if wire in digits[i]:
                signature[len(digits[i])] += 1
        signatures[wire] = ''.join(str(s) for s in signature)
    return signatures

def main():
    signatures = {v: k for k, v in compute_signatures(digits).items()}

    print(signatures)

    day1 = 0
    day2 = 0

    for line in aoc.lines(8):
        examples, code = line.split(' | ')
        examples = examples.split()
        code = code.split()

        for x in code:
            if len(x) in [len(digits[1]), len(digits[4]), len(digits[7]), len(digits[8])]:
                day1 += 1

        mapping = {ord(k): signatures[s] for (k, s) in compute_signatures(examples).items()}
        decoded = [''.join(sorted(x.translate(mapping))) for x in code]
        result = int(''.join(digit_map[x] for x in decoded))
        day2 += result


    print("Day 1: ", day1)
    print("Day 2: ", day2)

if __name__ == '__main__':
    main()
