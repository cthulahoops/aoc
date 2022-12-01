import aoc

input_data = aoc.ints(1)

def windows(nums):
    return list(map(sum, zip(nums, nums[1:], nums[2:])))

def number_increases(nums):
    return len(list(filter(lambda pair: pair[1] > pair[0], zip(nums, nums[1:]))))

print(number_increases(input_data))
print(number_increases(windows(input_data)))
