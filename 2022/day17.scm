(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 match))
(use-modules (ice-9 q))

(define-immutable-record-type <block>
  (make-block position shape)
  block?
  (position block-position set-block-position)
  (shape block-shape))

(define (block-points block)
  (map (lambda (x) (point+ (block-position block) x)) (block-shape block)))

(define (block-y block) (point-y (block-position block)))

(define shapes (make-q))
(enq! shapes (list (make-point 0 0) (make-point 1 0) (make-point 2 0) (make-point 3 0)))
(enq! shapes (list (make-point 1 0) (make-point 0 1) (make-point 1 1) (make-point 2 1) (make-point 1 2)))
(enq! shapes (list (make-point 0 0) (make-point 1 0) (make-point 2 0) (make-point 2 1) (make-point 2 2)))
(enq! shapes (list (make-point 0 0) (make-point 0 1) (make-point 0 2) (make-point 0 3)))
(enq! shapes (list (make-point 0 0) (make-point 0 1) (make-point 1 0) (make-point 1 1)))

(define (get-shape!) (let ((shape (deq! shapes))) (enq! shapes shape) shape))
(define (spawn-point grid) (make-point 2 (+ 4 (tower-height grid))))
(define (tower-height grid) (hash-fold (lambda (k v m) (max m (point-y k))) 0 grid))

(define (collide-point grid point) (or (< (point-y point) 1) (< (point-x point) 0) (> (point-x point) 6) (hash-ref grid point)))
(define (collide grid block) (or-map (partial collide-point grid) (block-points block)))

(define (move grid block delta)
  (let ((new-block (set-block-position block (point+ (block-position block) delta))))
    (if (collide grid new-block) block new-block)))

(define (to-delta char)
  (match char (#\< (make-point -1 0))
              (#\> (make-point 1 0))))

(define (move-down grid block) (move grid block (make-point 0 -1)))
(define (add-to-grid! grid block) (for-each (lambda (p) (hash-set! grid p #\#)) (block-points block)))

(define (do-turn grid block input)
  ; (display "Input: ")
  ; (display input)
  ; (newline)
  (pipe>
    (move grid block (to-delta input))
    (move-down grid)))

(define (drop-block! grid input)
  (let loop ((block (spawn-block! grid)) (input input))
    (let ((new-block (do-turn grid block (car input))))
      ; (display new-block)
      ; (newline)
      (if (equal? (block-y new-block) (block-y block))
          (begin (add-to-grid! grid new-block)
                 (cdr input))
          (loop new-block (cdr input))))))

(define (spawn-block! grid) (make-block (spawn-point grid) (get-shape!)))

(define (display-grid grid)
  (let loop ((y (tower-height grid)))
    (display (list->string (map (lambda (x) (or (hash-ref grid (make-point x y)) #\.)) (range 0 7))))
    (newline)
    (if (> y 1) (loop (- y 1)))))

(define (part1)
  (let* ((grid (make-hash-table))
         (rock-count 2022)
         (input (string->list (read-line)))
         (input (append input input input))
         ; (input (string->list ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))
         )
    (let loop ((input input) (i 0))
      (if
        (< i rock-count)
        (begin
            ; (display i)
            ; (newline)
            (loop (drop-block! grid input) (1+ i)))
        input))
    (display-grid grid)
    (tower-height grid)
    ))

(define (part2) 0)
