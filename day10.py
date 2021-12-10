import aoc

PAIRS = dict(["()", "[]", "{}", "<>"])
ERROR_SCORES = {')': 3, ']': 57, '}': 1197, '>': 25137}
INCOMPLETE_SCORES = {')': 1, ']': 2, '}': 3, '>': 4}

def line_completion(line):
    stack = []
    for item in line:
        if item in PAIRS:
            stack.append(PAIRS[item])
        else:
            to_close = stack.pop()
            if to_close != item:
                raise SyntaxError(item)
    stack.reverse()
    return stack

def completion_score(remainder):
    score = 0
    for item in remainder:
        score *= 5
        score += INCOMPLETE_SCORES[item]
    return score

def main():
    part1 = 0
    part2_scores = []

    lines = aoc.lines(10)
    for line in lines:
        try:
            remainder = line_completion(line)
        except SyntaxError as err:
            part1 += ERROR_SCORES[err.args[0]]
            continue
        part2_scores.append(completion_score(remainder))

    print("Part 1: ", part1)
    part2_scores.sort()
    print("Part 2: ", part2_scores[len(part2_scores) // 2])

if __name__ == '__main__':
    main()
