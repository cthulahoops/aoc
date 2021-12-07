from collections import Counter
import aoc

def tri(n):
    return (n + 1) * n // 2

def cost(crabs, i):
    return sum(abs(k - i) * v for (k, v) in crabs.items())

def cost_tri(crabs, i):
    return sum(tri(abs(k - i)) * v for (k, v) in crabs.items())

def bisect(a, b, f):
    while b > a:
        middle = (a + b) // 2
        x0 = f(middle)
        if x0 < f(middle + 1):
            if x0 < f(middle - 1):
                return x0
            b = middle
        else:
            a = middle

def main():
    crabs = aoc.comma_ints(7)
    crabs.sort()
    left = crabs[0]
    right = crabs[-1]
    crabs = Counter(crabs)
    print("Part 1: ", bisect(left, right, lambda i: cost(crabs, i)))
    print("Part 2: ", bisect(left, right, lambda i: cost_tri(crabs, i)))

if __name__ == '__main__':
    main()
