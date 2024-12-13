from math import log10, floor
import numpy as np

from scipy import sparse
from scipy.sparse.linalg import matrix_power

def main():
    input = [125, 17]
    input = list(map(int, "773 79858 0 71 213357 2937 1 3998391".split()))
    graph = build_graph(input)
    size = len(graph)

    print("Graph size:", size)

    transitions = {v: i for i, v in enumerate(graph.keys())}

    input_vector = np.zeros(size)
    for i in input:
        input_vector[transitions[i]] += 1

    transition_matrix = build_matrix(graph, transitions)

    part1 = input_vector @ matrix_power(transition_matrix, 25)
    print("Part 1:", sum(part1))

    part2 = input_vector @ matrix_power(transition_matrix, 75)
    print("Part 2:", sum(part2))

    part2 = input_vector @ matrix_power(transition_matrix, 200)
    print("Part 2:", sum(part2))

def step_stone(n):
    if n == 0:
        return [1]
    digits = floor(log10(n)) + 1
    divisor = 10 ** (digits // 2)
    if digits % 2 == 0:
        return [n // divisor, n % divisor]
    return [n * 2024]


def build_graph(input):
    transitions = {}

    queue = input.copy()

    while queue:
        if len(transitions) >= 10000:
            raise Exception("Graph too large")
        node = queue.pop(0)
        if node in transitions:
            continue
        transitions[node] = step_stone(node)
        queue.extend(transitions[node])
    return transitions

def build_matrix(graph, transitions):
    size = len(graph)
    rows = []
    cols = []
    values = []

    for key, value in graph.items():
        for v in value:
            rows.append(transitions[key])
            cols.append(transitions[v])
            values.append(1)  # or += 1 if you want to count multiple transitions

    return sparse.csr_matrix(
        (values, (rows, cols)),
        shape=(size, size)
    )

def step(items):
    output = []
    for item in items:
        output.extend(step_stone(item))
    return output

def experim():
    input = list(map(int, "773 79858 0 71 213357 2937 1 3998391".split()))

    for i in range(20):
        input = step(input)

    print(sorted(input))

    graph = build_graph(input)
    print(len(graph))



if __name__ == "__main__":
    #main()
    experim()
