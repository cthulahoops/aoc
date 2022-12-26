(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (ice-9 q))
(use-modules (aoc))

(define (read-input) (map (lambda (line) (apply make-point3 (map string->number (string-split line #\,)))) (read-lines)))

(define (list->set items) (let ((set (make-hash-table))) (for-each (lambda (item) (hash-set! set item #t)) items) set))

(define (uncovered scan point)
  (count-where (lambda (p) (not (hash-ref scan p))) (neighbours point)))
(define (covered scan point)
  (count-where (lambda (p) (hash-ref scan p)) (neighbours point)))

(define (fillable? water scan p)
  (not (or (< (point3-x p) -1)
           (> (point3-x p) 22)
           (< (point3-y p) -1)
           (> (point3-y p) 22)
           (< (point3-z p) -1)
           (> (point3-z p) 22)
           (hash-ref scan p)
           (hash-ref water p))))

(define (flood-fill scan start)
  (let* ((water (make-hash-table))
         (q (make-q))
         (make-water! (lambda (item) (hash-set! water item #t) (enq! q item))))
    (make-water! start)
    (while (not (q-empty? q))
      (let ((current (deq! q)))
        (for-each make-water! (filter (partial fillable? water scan) (neighbours current)))))
    water))

(define (part1)
  (let* ((input (read-input))
         (scan (list->set input)))
  (sum (map (partial uncovered scan) input))))

(define (part2)
  (let* ((input (read-input))
         (scan (list->set input))
         (water (flood-fill scan (make-point3 -1 -1 -1))))
    (sum (map (partial covered water) input))))

(define offsets
  (list
    (make-point3 1 0 0)
    (make-point3 -1 0 0)
    (make-point3 0 1 0)
    (make-point3 0 -1 0)
    (make-point3 0 0 1)
    (make-point3 0 0 -1)
    ))

(define (neighbours p) (map (partial point3+ p) offsets))
