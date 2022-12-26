(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (aoc))

(define (blocks) (map (lambda (line) (map string->number line)) (read-blocks)))
(define (sorted-totals) (sort (map sum (blocks)) >))

(define (part1) (car (sorted-totals)))
(define (part2) (sum (take (sorted-totals) 3)))
