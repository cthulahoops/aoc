#!/bin/bash

possum -p 'many_sep(number, nl)' <(for line in $(cat input/5-pages); do
     possum day5.possum <(cat input/5-header; echo $line)
done)
