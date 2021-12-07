def input_file(day, example=False):
    return f"input/day{day}{'-example' if example else ''}"

def words(day):
    return [line.split() for line in lines(day)]

def ints(day):
    return [int(line) for line in lines(day)]

def comma_ints(day):
    return [int(x) for x in lines(day)[0].split(',')]

def lines(day):
    return open(f"input/day{day}").readlines()

def blocks(day, example=False):
    return open(input_file(day, example)).read().split("\n\n")
