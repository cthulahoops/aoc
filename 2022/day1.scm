(add-to-load-path ".")
(use-modules (ice-9 format))
(use-modules (srfi srfi-1))
(use-modules (aoc))

(define (keep-biggest new items) (cdr (sort (cons new items) <)))

(define blocks
  (pipe>
    (with-input-from-file "input/1" read-blocks)
    (map (lambda (line) (map string->number line)))))

(define sorted-totals (sort (map sum blocks) >))

(format #t "Part 1: ~d\n" (car sorted-totals))
(format #t "Part 2: ~d\n" (sum (take sorted-totals 3)))
