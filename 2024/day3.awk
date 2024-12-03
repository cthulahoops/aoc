#!/bin/awk -f

BEGIN {
   total = 0
}
{
    str = $0
    while (match(str, /mul\(([0-9]+),([0-9]+)\)/, arr)) {
        total += arr[1] * arr[2]
        str = substr(str, RSTART + RLENGTH)
    }
}
END {
    print total
}
