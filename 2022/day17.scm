(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
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

(define shapes (make-hash-table))
(hash-set! shapes 0 (list (make-point 0 0) (make-point 1 0) (make-point 2 0) (make-point 3 0)))
(hash-set! shapes 1 (list (make-point 1 0) (make-point 0 1) (make-point 1 1) (make-point 2 1) (make-point 1 2)))
(hash-set! shapes 2 (list (make-point 0 0) (make-point 1 0) (make-point 2 0) (make-point 2 1) (make-point 2 2)))
(hash-set! shapes 3 (list (make-point 0 0) (make-point 0 1) (make-point 0 2) (make-point 0 3)))
(hash-set! shapes 4 (list (make-point 0 0) (make-point 0 1) (make-point 1 0) (make-point 1 1)))

(define (get-shape shape-idx) (hash-ref shapes shape-idx))
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

(define (drop-block! grid input shape-idx)
  (let loop ((block (spawn-block! grid shape-idx)) (input input))
    (let ((new-block (do-turn grid block (circular-car input))))
      ; (display new-block)
      ; (newline)
      (if (equal? (block-y new-block) (block-y block))
          (begin (add-to-grid! grid new-block)
                 (circular-cdr input))
          (loop new-block (circular-cdr input))))))

(define (spawn-block! grid shape-idx) (make-block (spawn-point grid) (get-shape shape-idx)))

(define (display-grid grid)
  (newline)
  (let loop ((y (tower-height grid)))
    (display (list->string (map (lambda (x) (or (hash-ref grid (make-point x y)) #\.)) (range 0 7))))
    (newline)
    (if (and (> y 1) (> y (- (tower-height grid) 10))) (loop (- y 1))))
  (newline))

(define-immutable-record-type <circular>
  (make-circular offset head tail)
  circular?
  (offset circular-offset)
  (head circular-head)
  (tail circular-tail))

(define (circular lst) (make-circular 0 lst '()))
(define (circular-car circular) (car (circular-head circular)))
(define (circular-cdr circular)
  (if (null? (cdr (circular-head circular)))
        (make-circular 0
                       (reverse (cons (car (circular-head circular)) (circular-tail circular)))
                       (list))
        (make-circular (1+ (circular-offset circular))
                       (cdr (circular-head circular))
                       (cons (car (circular-head circular)) (circular-tail circular)))))

(define (simulate rock-count input)
  (let* ((grid (make-hash-table)))
    (let loop ((input input) (i 0))
      (if
        (< i rock-count)
        (begin
            ; (display i)
            ; (display " ")
            ; (display (tower-height grid))
            ; (newline)
            (loop (drop-block! grid input (modulo i 5)) (1+ i)))
        ))
    (tower-height grid)
    ))

(define (read-input) (circular (string->list (read-line))))

(define (part1) (simulate 2022 (read-input)))

(define (make-cycle end-step end-height start-step start-height)
  (cons (- end-step start-step) (- end-height start-height)))
(define (cycle-length cycle) (car cycle))
(define (cycle-height cycle) (cdr cycle))

(define (find-cycle input)
  (let* ((grid (make-hash-table))
         (visited (make-hash-table)))
    (let loop ((input input) (i 0))
        (if (and (> i 1730) ; There's an early cycle that doesn't work!
                 (hash-ref visited (list (circular-offset input) (modulo i 5))))
           (apply make-cycle (append (list i (tower-height grid)) (hash-ref visited (list (circular-offset input) (modulo i 5)))))
           (begin (hash-set! visited (list (circular-offset input) (modulo i 5)) (list i (tower-height grid)))
                  (loop (drop-block! grid input (modulo i 5)) (1+ i)))))))

; Originally
; >>> (1_000_000_000_000 % 1700)
; 200
; >>> 318 + (1_000_000_000_000 // 1700) * 2623
; 1542941176480
(define (part2)
  (let* ((input (read-input))
         (cycle (find-cycle input))
         (rocks (modulo 1000000000000 (cycle-length cycle)))
         (cycle-count (truncate (/ 1000000000000 (cycle-length cycle))))
         (rocks-height (simulate rocks input)))
    (+ rocks-height (* cycle-count (cycle-height cycle)))))
