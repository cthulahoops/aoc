import aoc
from collections import Counter

timers = [int(x) for x in aoc.lines(6)[0].split(',')]
counts = Counter(timers)
counts = [counts[x] for x in range(9)]

def step(counts):
    return counts[1:7] + [counts[7] + counts[0], counts[8], counts[0]]

def simulate(counts, steps):
    for steps in range(steps):
        counts = step(counts)
    return counts

print("Part 1: ", sum(simulate(counts, 80)))
print("Part 2: ", sum(simulate(counts, 256)))
