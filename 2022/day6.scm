(add-to-load-path ".")
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 format))
(use-modules (aoc))

(define (at-start? items packet-length) (all-distinct (list-head items packet-length)))

(define (all-distinct items)
  (distinct-neighbours (sort items char<?)))

(define (distinct-neighbours items)
  (if (null? (cdr items))
      #t
      (and (not (eqv? (car items) (cadr items))) (distinct-neighbours (cdr items)))))

(define (find-start packet-length items)
  (let loop ((count 0) (items items))
    (if (at-start? items packet-length) count (loop (+ count 1) (cdr items)))))

(define (part1)
  (pipe>
    (with-input-from-file "input/6" read-line)
    (string->list)
    (find-start 4)
    (+ 4)))

(define (part2)
  (pipe>
    (with-input-from-file "input/6" read-line)
    (string->list)
    (find-start 14)
    (+ 14)))

(format #t "Part 1: ~s\n" (part1))
(format #t "Part 2: ~s\n" (part2))
