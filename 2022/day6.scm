(use-modules (ice-9 rdelim))
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

(define (find-marker packet-length)
  (pipe>
    (read-line)
    (string->list)
    (find-start packet-length)
    (+ packet-length)))

(define (part1) (find-marker 4))
(define (part2) (find-marker 14))
