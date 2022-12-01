seats = [int(row.strip().replace('F', '0').replace('B', '1').replace('R', '1').replace('L', '0'), base=2)
    for row in open('input/day5')]

print(max(seats))
for x in range(min(seats), max(seats)):
    if x not in seats:
        print(x)
