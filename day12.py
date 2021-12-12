from collections import defaultdict
import aoc

def build_graph(connections):
    caves = defaultdict(list)
    for (k, v) in connections:
        caves[k].append(v)
        caves[v].append(k)
    caves['end'] = []

    return caves

def is_small(cave):
    return all(x.islower() for x in cave)

def paths(caves, current, visited, spare_time=0):
    if current == 'end':
        yield ()
        return

    new_visited = visited | {current}

    for option in caves[current]:
        time_spent = 0
        if is_small(option) and option in visited:
            if not spare_time or option == 'start':
                continue
            time_spent = 1

        for tail in paths(caves, option, new_visited, spare_time - time_spent):
            yield (current,) + tail

def main():
    connections = [path.split('-') for path in aoc.lines(12)]
    caves = build_graph(connections)

    part1_paths = list(paths(caves, 'start', set()))
    print("Part 1: ", len(part1_paths))

    part2_paths = list(paths(caves, 'start', set(), 1))
    print("Part 2: ", len(part2_paths))

if __name__ == '__main__':
    main()
