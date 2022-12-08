(use-modules (srfi srfi-1))
(use-modules (aoc))

(define (keep-biggest new items) (cdr (sort (cons new items) <)))

(define (blocks)
  (pipe>
    (read-blocks)
    (map (lambda (line) (map string->number line)))))

(define (sorted-totals) (sort (map sum (blocks)) >))

(define (part1) (car (sorted-totals)))
(define (part2) (sum (take (sorted-totals) 3)))
