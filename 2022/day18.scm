(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))
(use-modules (aoc))

(define-immutable-record-type <point3>
  (make-point3 x y z)
  point3?
  (x point-x set-point-x)
  (y point-y set-point-y)
  (z point-z set-point-z))

(define (point3+ p1 p2) (make-point3 (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2)) (+ (point-z p1) (point-z p2))))

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

(define (read-input) (map (lambda (line) (apply make-point3 (map string->number (string-split line #\,)))) (read-lines)))

(define (list->set items) (let ((set (make-hash-table))) (for-each (lambda (item) (hash-set! set item #t)) items) set))

(define (uncovered scan point)
  (- 6 (count-where (partial hash-ref scan) (neighbours point))))

(define (part1)
  (let* ((input (read-input))
         (scan (list->set input)))
  (sum (map (partial uncovered scan) input))))

(define (part2) 0)
