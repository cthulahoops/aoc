#!/bin/bash

function adjacent() {
    read -a arr
    paste <(printf "%s\n" "${arr[@]}" | head -n -1) <(printf "%s\n" "${arr[@]}" | tail +2)
}

function sub() {
    while read a b; do echo $(( $b - $a )); done
}

function min_max() {
    local sorted=($(sort -n))
    echo "${sorted[0]} ${sorted[${#sorted[@]}-1]}"
}

function is_safe() {
    local ab=($(adjacent | sub | min_max))
    local a=${ab[0]}
    local b=${ab[1]}

    if (( $a >= -3 )) && (( $b <= -1 )) || (( $a >= 1 )) && (( $b <= 3 )); then
        return 0
    fi

    return 1
}

function part1_lines() {
    while read line; do
        if echo $line | is_safe; then
            echo $line
        fi
    done
}

function part2_lines() {
    while read line; do
        if echo $line | is_safe \
                || echo $line | cut -d' ' -f 2- | is_safe \
                || echo $line | cut -d' ' -f 1,3- | is_safe \
                || echo $line | cut -d' ' -f -2,4- | is_safe \
                || echo $line | cut -d' ' -f -3,5- | is_safe \
                || echo $line | cut -d' ' -f -4,6- | is_safe \
                || echo $line | cut -d' ' -f -5,7- | is_safe \
                || echo $line | cut -d' ' -f -6,8- | is_safe \
                || echo $line | cut -d' ' -f -7,9- | is_safe \
                || echo $line | cut -d' ' -f -8,10- | is_safe \
                || echo $line | cut -d' ' -f -9,11- | is_safe \
                || echo $line | cut -d' ' -f -10,12- | is_safe \
                ; then
            echo $line
        fi
    done
}

cat "$1" | part2_lines | wc -l
