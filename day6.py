import aoc
from collections import Counter

counts = Counter(aoc.comma_ints(6))
counts = [counts[x] for x in range(9)]

def memoize(f):
    cache = {}
    def wraps(n):
        try:
            return cache[n]
        except KeyError:
            r = cache[n] = f(n)
            return r
    return wraps

first_values = [sum(counts) + sum(counts[:i]) for i in range(9)]

@memoize
def fish(n):
    if n < 9:
        return first_values[n]
    return fish(n - 7) + fish(n - 9)

print("Part 1: ", fish(80))
print("Part 2: ", fish(256))
