(add-to-load-path ".")
(use-modules (aoc))
(use-modules (srfi srfi-1))
(use-modules (ice-9 regex))

(define (read-input) (map parse-line (read-lines)))
(define (match-number match count) (string->number (match:substring match count)))


(define (manhatten p1 p2) (let ((p-diff (point- p1 p2))) (+ (abs (point-x p-diff)) (abs (point-y p-diff)))))

(define (parse-line line)
  (let ((match (string-match "=([-0-9]*).*=([-0-9]*).*=([-0-9]*).*=([-0-9]*)" line)))
    (list
      (make-point (match-number match 1) (match-number match 2))
      (make-point (match-number match 3) (match-number match 4)))))

(define (impossible-range y sensor beacon)
  (let* ((distance (manhatten sensor beacon))
         (dy (abs (- (point-y sensor) y)))
         (dx (- distance dy)))
      (make-range (- (point-x sensor) dx) (+ (point-x sensor) dx))
      ))

(define (merge-range-pair a b)
  (make-range (min (range-start a) (range-start b)) (max (range-end a) (range-end b))))

(define (merge-ranges ranges)
  (let loop ((ranges ranges) (result (list)))
    (cond 
      ((null? (cdr ranges)) (reverse (cons (first ranges) result)))
      ((range-overlaps? (first ranges) (second ranges))
       (loop (cons (merge-range-pair (first ranges) (second ranges)) (cddr ranges)) result))
      (else (loop (cdr ranges) (cons (first ranges) ranges)))
        )))

(define (non-empty-range range) (>= (range-end range) (range-start range)))

(define (part1)
  (let* ((target-y 2000000)
         (sensor-report (read-input))
         (merged (covered-at sensor-report target-y))
         (beacon-count (count-unique (map (compose point-x cadr) (filter (lambda (pair) (= target-y (point-y (cadr pair)))) sensor-report))))
         )
    (display beacon-count)
    (display merged)
    (newline)
    (- (sum (map range-length merged)) beacon-count)
    ))

(define (covered-at sensor-report target-y)
  (let ((ranges (filter non-empty-range (map (lambda (pair) (impossible-range target-y (car pair) (cadr pair))) sensor-report))))
    (merge-ranges (sort ranges (lambda (x y) (< (range-start x) (range-start y)))))))

(define (part2)
  (let* (
         (sensor-report (read-input))
         (merged covered-at sensor-report target-y)
         )
    0
    ))
