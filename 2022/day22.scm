(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (aoc))
(use-modules (grid))

(define-record-type <position>
  (make-position location facing)
  position?
  (location position-location)
  (facing position-facing))

; (define assembley-plan (list 5 4 1 2 2 3 3 1 4 5 6 6 7 7))
; (define cube-size 50)
; EXAMPLE
(define assembley-plan-2d (list 1 2 3 4 5 4 1 5 6 7 3 7 6 2))
(define assembley-plan (list 1 2 3 3 2 4 5 6 6 5 4 1 7 7))
(define cube-size 4)
; REAL
; (define assembley-plan (list 1 2 3 4 4 3 5 5 2 1 6 7 7 6))
; (define cube-size 50)

(define (part1)
  (let* [(grid (read-board))
         (instructions (parse-instructions (car (read-block))))
         (start-position (make-position (make-point (min-x grid 1) 1) 0))
         (boundary (assemble assembley-plan-2d (follow-boundary grid (position-location start-position) (make-point 1 0) 14)))
         (bounds (create-boundary boundary))
         (final (fold (lambda (instruction position) (apply-instruction grid bounds position instruction)) start-position instructions))
         ]
    (display-grid (lambda (x) (or x #\space)) grid)
    (password final)
    ))

(define (part2)
  (let* [(grid (read-board))
         (instructions (parse-instructions (car (read-block))))
         (start-position (make-position (make-point (min-x grid 1) 1) 0))
         (boundary (assemble assembley-plan (follow-boundary grid (position-location start-position) (make-point 1 0) 14)))
         (bounds (create-boundary boundary))
         (final (fold (lambda (instruction position) (apply-instruction grid bounds position instruction)) start-position instructions))
         ]
    (password final)
    ))

(define (parse-instructions input)
  (let loop ((input (string->list input)) (result (list)))
    (match (split-number input) ((step-count (turn . rest)) (loop rest (cons turn (cons step-count result))))
                                ((step-count '()) (reverse (cons step-count result))))))

(define (split-number input)
  (let* ((digits (take-while char-numeric? input)))
    (list (string->number (list->string digits)) (drop input (length digits)))))

(define (read-board) (read-grid (match-lambda (#\space #f) (c c))))

(define (min-x grid y)
  (let loop ((x 1))
    (if (hash-ref grid (make-point x y))
        x
        (loop (+ x 1)))))

(define (ahead-of bounds position)
  (match (hash-ref bounds position)
         (#f (make-position (point+ (position-location position) (step-direction (position-facing position))) (position-facing position)))
         ((? position? wrap-position) wrap-position)))

(define-match step-direction
  (0 (make-point 1 0))
  (1 (make-point 0 1))
  (2 (make-point -1 0))
  (3 (make-point 0 -1)))

(define-match vec-to-facing
  (($ <point> 1 0) 0)
  (($ <point> 0 1) 1)
  (($ <point> -1 0) 2)
  (($ <point> 0 -1) 3))

(define (step grid bounds position)
  (let ((next (ahead-of bounds position)))
    (match (hash-ref grid (position-location next)) (#\. next)
                                                    (#\# position))))

(define (turn position count) (make-position (position-location position) (modulo (+ (position-facing position) count) 4)))
(define-match* apply-instruction
   ((grid bounds position #\L) (turn position -1))
   ((grid bounds position #\R) (turn position 1))
   ((grid bounds position 0) position)
   ((grid bounds position (? number? step-count)) (apply-instruction grid bounds (step grid bounds position) (- step-count 1))))

(define (password position)
  (+ (* 1000 (point-y (position-location position))) (* 4 (point-x (position-location position))) (position-facing position)))

(define (iterate-n f init count)
  (if (= 0 count)
      '()
      (cons init (iterate-n f (f init) (1- count)))))

(define (turn-left vec) (step-direction (modulo (- (vec-to-facing vec) 1) 4)))
(define (turn-right vec) (step-direction (modulo (+ (vec-to-facing vec) 1) 4)))

(define (choose-next grid point vec)
  (let* (
        (left (turn-left vec))
        (right (turn-right vec))
        (ahead (point+ point vec))
        (inside-left (point+ ahead left))
        (outside-right (point+ point right)))
    (cond
      ((hash-ref grid inside-left) (cons inside-left left))
      ((hash-ref grid ahead) (cons ahead vec))
      ((hash-ref grid outside-right) (cons point right)))))


(define (follow-boundary grid start vec n)
  (if (= n 0)
    '()
    (let* ((first-edge (iterate-n (lambda (p) (point+ p vec)) start cube-size))
           (next (choose-next grid (last first-edge) vec))
           (next-start (car next))
           (next-vec (cdr next)))
      (cons (map (partial list (turn-right vec)) first-edge) (follow-boundary grid next-start next-vec (- n 1))))))

(define (assemble plan boundary)
  (pipe>
    (sort (map cons plan boundary) (lambda (x y) (< (car x) (car y))))
    (map cdr)
    (chunk 2)
    (append-map (lambda (parts) (map list (first parts) (reverse (second parts)))))
    )
  )

(define (point-back point) (make-point (- (point-x point)) (- (point-y point))))

(define (create-boundary boundary)
  (let* ((bounds (make-hash-table))
         (add-bound! (lambda (p1 v1 p2 v2) (hash-set! bounds (make-position p1 (vec-to-facing v1)) (make-position p2 (vec-to-facing v2))))))
    (map (match-lambda (((v1 p1) (v2 p2)) (add-bound! p1 (point-back v1) p2 v2)
                                          (add-bound! p2 (point-back v2) p1 v1))) boundary)
    bounds))
