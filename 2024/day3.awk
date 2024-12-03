#!/bin/awk -f

BEGIN {
   part1 = 0

   enabled = 1
   part2 = 0
}
{
    str = $0
    while (match(str, /mul\(([0-9]+),([0-9]+)\)/, arr)) {
        part1 += arr[1] * arr[2]
        str = substr(str, RSTART + RLENGTH)
    }

    str = $0
    while (match(str, /mul\(([0-9]+),([0-9]+)\)|do\(\)|don't\(\)/, arr)) {
        if (arr[0] == "do()") {
            enabled = 1
        } else if (arr[0] == "don't()") {
            enabled = 0
        } else if (enabled) {
            part2 += arr[1] * arr[2]
        }
        str = substr(str, RSTART + RLENGTH)
    }
}
END {
    print part1
    print part2
}
