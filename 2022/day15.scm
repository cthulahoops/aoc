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

(define (range-mergable? a b) (>= (+ (range-end a) 1) (range-start b)))

(define (merge-ranges ranges)
  (let loop ((ranges ranges) (result (list)))
    (cond 
      ((null? (cdr ranges)) (reverse (cons (first ranges) result)))
      ((range-mergable? (first ranges) (second ranges))
       (loop (cons (merge-range-pair (first ranges) (second ranges)) (cddr ranges)) result))
      (else (loop (cdr ranges) (cons (first ranges) result)))
        )))

(define (non-empty-range range) (>= (range-end range) (range-start range)))

(define (part1)
  (let* (
         (sensor-report (read-input))
         (target-y (if (= (length sensor-report) 14) 10 2000000))
         (merged (covered-at sensor-report target-y))
         (beacon-count (count-unique (map (compose point-x cadr) (filter (lambda (pair) (= target-y (point-y (cadr pair)))) sensor-report))))
         )
    (- (sum (map range-length merged)) beacon-count)
    ))

(define (covered-at sensor-report target-y)
  (let ((ranges (filter non-empty-range (map (lambda (pair) (impossible-range target-y (car pair) (cadr pair))) sensor-report))))
    (merge-ranges (sort ranges (lambda (x y) (< (range-start x) (range-start y)))))))

(define (search-loop max-y sensor-report)
  (let loop ((y 0))
    (if (= 0 (modulo y 100000)) (begin (display y) (newline)))
    (let ((cover (covered-at sensor-report y)))
      (if (> (length cover) 1)
          (cons y cover)
          (if (< y max-y) (loop (+ y 1)))
          ))))

(define (part2)
  (let* (
         (sensor-report (read-input))
         (max-y (if (= (length sensor-report) 14) 20 4000000))
         (result (search-loop max-y sensor-report))
         (distress-y (car result))
         (distress-x (+ 1 (range-end (car (cdr result))))))
      (+ (* 4000000 distress-x) distress-y)
    ))
