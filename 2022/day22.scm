(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (aoc))

(define-record-type <position>
  (make-position location facing)
  position?
  (location position-location)
  (facing position-facing))

(define (parse-instructions input)
  (let loop ((input (string->list input)) (result (list)))
    (match (split-number input) ((step-count (turn . rest)) (loop rest (cons turn (cons step-count result))))
                                ((step-count '()) (reverse (cons step-count result))))))

(define (split-number input)
  (let* ((digits (take-while char-numeric? input)))
    (list (string->number (list->string digits)) (drop input (length digits)))))


(define (parse-board-line grid) 
  (match-lambda ((y . line)
     (map (match-lambda ((x . #\space) x)
                        ((x . c) (hash-set! grid (make-point x y) c))) (enumerate (string->list line))))))

(define (parse-board board)
  (let ((grid (make-hash-table)))
    (map (parse-board-line grid) (enumerate board))
    grid))

(define (min-x grid y)
  (let loop ((x 1))
    (if (hash-ref grid (make-point x y))
        x
        (loop (+ x 1)))))

(define (max-x grid y x0)
  (let loop ((x x0))
    (if (hash-ref grid (make-point x y))
        (loop (+ x 1))
        (- x 1))))

(define (min-y grid x)
  (let loop ((y 1))
    (if (hash-ref grid (make-point x y))
        y
        (loop (+ y 1)))))

(define (max-y grid x y0)
  (let loop ((y y0))
    (if (hash-ref grid (make-point x y))
        (loop (+ y 1))
        (- y 1))))

(define (hash-keys hash) (hash-map->list (lambda (k v) k) hash))

(define (compute-bounds grid)
  (let ((bounds (make-hash-table))
        (grid-max-y (maximum (map point-y (hash-keys grid))))
        (grid-max-x (maximum (map point-x (hash-keys grid)))))
    (do ((y 1 (1+ y)))
        ((> y grid-max-y))
        (let* ((x0 (min-x grid y)) (x1 (max-x grid y x0)))
          (hash-set! bounds (cons (make-point x0 y) (make-point -1 0)) (make-point x1 y))
          (hash-set! bounds (cons (make-point x1 y) (make-point 1 0)) (make-point x0 y))
        ))
    (do ((x 1 (1+ x)))
        ((> x grid-max-x))
        (let* ((y0 (min-y grid x)) (y1 (max-y grid x y0)))
          (display (list x y0 y1))
          (newline)
          (hash-set! bounds (cons (make-point x y0) (make-point 0 -1)) (make-point x y1))
          (hash-set! bounds (cons (make-point x y1) (make-point 0 1)) (make-point x y0))
        ))
    bounds
    ))

(define (bounds+ bounds point step)
  (match (hash-ref bounds (cons point step)) (#f (point+ point step))
                                             ((? point? wrap-point) wrap-point)))

(define step-direction
  (match-lambda (0 (make-point 1 0))
                (1 (make-point 0 1))
                (2 (make-point -1 0))
                (3 (make-point 0 -1))))

(define (ahead-of bounds position)
  (bounds+ bounds (position-location position) (step-direction (position-facing position))))

(define (step grid bounds position)
  (let ((next (ahead-of bounds position)))
    ; (display (list 'next next))
    ; (newline)
    (match (hash-ref grid next) (#\. (make-position next (position-facing position)))
                                (#\# position))))

(define (turn position count) (make-position (position-location position) (modulo (+ (position-facing position) count) 4)))
(define apply-instruction
  (match-lambda* ((grid bounds position #\L) (turn position -1))
                 ((grid bounds position #\R) (turn position 1))
                 ((grid bounds position 0) position)
                 ((grid bounds position (? number? step-count)) (apply-instruction grid bounds (step grid bounds position) (- step-count 1)))))

(define (password position)
  (+ (* 1000 (point-y (position-location position))) (* 4 (point-x (position-location position))) (position-facing position)))

(define (part1)
  (let* (
        (grid (parse-board (read-block)))
        (bounds (compute-bounds grid))
        (instructions (parse-instructions (car (read-block))))
        (position (make-position (make-point (min-x grid 1) 1) 0))
        (final (fold (lambda (instruction position) (display (list position instruction)) (newline) (apply-instruction grid bounds position instruction)) position instructions))
        )
    (password final)
    ))

(define (part2) 0)
