import itertools
from collections import Counter
import aoc

def count_transitions(string):
    return Counter(string[i:i+2] for i in range(len(string)))

def step(string, rules):
    output = []
    for (i, e) in enumerate(string):
        output.append(e)
        output.append(rules.get(string[i:i+2], ""))

    return ''.join(output)

def step_trans(transitions, rules):
    output = Counter()
    for (transition, count) in transitions.items():
        try:
            insertion = rules[transition]
        except KeyError:
            output[transition] += count
        else:
            output[transition[0] + insertion] += count
            output[insertion + transition[1]] += count
    return output

def most_minus_least(counts):
    sorted_counts = counts.most_common()
    return sorted_counts[0][1] - sorted_counts[-1][1]

def to_element_counts(transition_counts):
    element_counts = Counter()
    for (k, v) in transition_counts.items():
        element_counts[k[0]] += v
    return element_counts

def iterate(f, x):
    while True:
        x = f(x)
        yield x

def take(it, n):
    return list(itertools.islice(it, 0, n))

def main():
    template, rules = aoc.blocks(14)
    rules = dict(line.split(' -> ') for line in rules.splitlines())

    print(template)
    print(rules)

    steps = take(iterate(lambda x: step(x, rules), template), 10)

    print("Part 1: ", most_minus_least(Counter(steps[-1])))

    t_counts = count_transitions(template)
    t_steps = take(iterate(lambda x: step_trans(x, rules), t_counts), 40)

    for i in range(10):
        assert count_transitions(steps[i]) == t_steps[i]

    print("Part 2: ", most_minus_least(to_element_counts(t_steps[-1])))


if __name__ == '__main__':
    main()
