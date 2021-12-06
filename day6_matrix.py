import aoc
from collections import Counter
import numpy

def exp(matrix, n):
    if n == 1:
        return matrix
    if n % 2:
        return matrix @ exp(matrix, n - 1)
    m = exp(matrix, n // 2)
    return m @ m

step = numpy.zeros((9,9), int)
for x in range(1,9):
    step[x-1, x] = 1
    step[6, 0] = 1
    step[8, 0] = 1

timers = [int(x) for x in aoc.lines(6)[0].split(',')]
counts = Counter(timers)
counts = numpy.array([counts[x] for x in range(9)])

print("Part 1: ", sum(exp(step, 80) @ counts))
print("Part 2: ", sum(exp(step, 256) @ counts))
