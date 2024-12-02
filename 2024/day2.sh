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

function safe() {
    while read a b; do
        if (( $a >= -3 )) && (( $b <= -1 )) || (( $a >= 1 )) && (( $b <= 3 )); then
            echo $a $b
        fi
    done
}

function part1_lines() {
    while read line; do
        echo $line | adjacent | sub | min_max
    done
}

cat "$1" | part1_lines| safe | wc -l
