(define-module (grid)
               #:export (read-grid))

(use-modules (ice-9 match))
(use-modules (aoc))

(define (parse-grid f board)
  (let* ((grid (make-hash-table))
         (parse-grid-cell (match-lambda* ((y (x . c)) (let ((grid-value (f c))) (if grid-value (hash-set! grid (make-point x y) grid-value))))))
         (parse-grid-line (match-lambda ((y . line) (map (partial parse-grid-cell y) (enumerate (string->list line)))))))
    (map parse-grid-line (enumerate board))
    grid))

(define (read-grid f) (parse-grid f (read-block)))
