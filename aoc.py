def words(day):
    return [line.split() for line in lines(day)]

def ints(day):
    return [int(line) for line in lines(day)]

def lines(day):
    return open(f"input/day{day}").readlines()
