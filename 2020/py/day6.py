import string

print("Part 1")

total = 0

for group in open("input/day6").read().split("\n\n"):
    answers = set()
    for char in group:
        if char in string.ascii_lowercase:
            answers.add(char)
    total += len(answers)

print(total)

print("Part 2")

total = 0

for group in open("input/day6").read().split("\n\n"):
    print("Group.")
    answers = None
    for line in group.split("\n"):
        if not line:
            continue

        my_answers = set()
        for char in line:
            if char in string.ascii_lowercase:
                my_answers.add(char)

        if answers is not None:
            answers = answers.intersection(my_answers)
        else:
            answers = my_answers

    total += len(answers)

print(total)
