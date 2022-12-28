(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (ice-9 q))
(use-modules (aoc))
(use-modules (grid))

(define-record-type <position>
  (make-position location facing)
  position?
  (location position-location)
  (facing position-facing))

(define (part1)
  (let* [(grid (read-board))
         (instructions (parse-instructions (car (read-block))))
         (start-position (make-position (make-point (min-x grid 1) 1) 0))]
    (follow-instructions grid instructions start-position wrap-around-edges)
    ))

(define (part2)
  (let* [(grid (read-board))
         (instructions (parse-instructions (car (read-block))))
         (start-position (make-position (make-point (min-x grid 1) 1) 0))
         ]
    (follow-instructions grid instructions start-position (segments-match-in-3d grid start-position))
    ))

(define (follow-instructions grid instructions start-position matching-function)
  (let* [(boundary-segments (follow-boundary grid (position-location start-position) (make-point 1 0) 14))
         (boundary (assemble matching-function boundary-segments))
         (bounds (create-boundary boundary))
         (final (fold (lambda (instruction position) (apply-instruction grid bounds position instruction)) start-position instructions))]
    (password final)))


(define (segments-match-in-3d grid start-position)
  (let* [(cube-size (cube-size grid))
         (folding-plan (fold-cube cube-size (lambda (p) (hash-ref grid p)) (position-location start-position)))]
  (lambda (a b)
    (equal?
      (map (partial plan-point-to-point3 cube-size folding-plan) (segment-ends a))
      (reverse (map (partial plan-point-to-point3 cube-size folding-plan) (segment-ends b)))))))

(define (wrap-around-edges a b)
  (let [(a (segment-ends a))
        (b (reverse (segment-ends b)))]
    (or (and (vertical? a) (vertical? b) (same-height? a b))
        (and (horizontal? a) (horizontal? b) (same-column? a b)))))

(define (vertical? segment) (= (point-x (first segment)) (point-x (second segment))))
(define (horizontal? segment) (= (point-y (first segment)) (point-y (second segment))))
(define (same-height? segment1 segment2) (= (point-y (first segment1)) (point-y (first segment2))))
(define (same-column? segment1 segment2) (= (point-x (first segment1)) (point-x (first segment2))))

(define (pair-by f items)
  (if (null? items)
    '()
    (let [(pair (find (lambda (b) (f (car items) b)) (cdr items)))]
        (cons (list (car items) pair) (pair-by f (delete pair (cdr items)))))))

(define (fold-cube cube-size on-cube? start-position)
  (visit-all (next-states cube-size on-cube?) (list start-position (make-point3 0 0 0) (make-point3 1 0 0) (make-point3 0 1 0))))

(define-match (next-states cube-size on-cube?)
              [(position position-3d right down)
               (let [(back (cross-product down right))
                     (forward (cross-product right down))]
                 (filter (compose on-cube? car)
                         (list (list (point+ position (make-point cube-size 0)) (point3+ position-3d right) back down)
                               (list (point- position (make-point cube-size 0)) (point3+ position-3d back) forward down)
                               (list (point+ position (make-point 0 cube-size)) (point3+ position-3d down) right back)
                               (list (point- position (make-point 0 cube-size)) (point3+ position-3d back) right forward))))])


(define (segment-ends segment) (map second (list (first segment) (last segment))))

(define (plan-point-to-point3 cube-size folding-plan point)
  (let* [(offset (point-face-offset cube-size point))
         (face-top-left (point- point offset))
         (y (point-y offset))
         (x (point-x offset))]
    (match (hash-ref folding-plan face-top-left)
           ((position-3d right down) (point3+ (point3* (- cube-size 1) position-3d) (point3* x right) (point3* y down))))))

(define-match* point-face-offset
              [(cube-size ($ <point> x y)) (make-point (floor-remainder (- x 1) cube-size) (floor-remainder (- y 1) cube-size))])

(define (visit-all next-states start-state)
  (let [(queue (enq! (make-q) start-state))
        (faces (make-hash-table))]
    (while
      (not (q-empty? queue))
      (let [(state (q-pop! queue))]
        (if (hash-ref faces (car state)) (continue))
        (hash-set! faces (car state) (cdr state))
        (for-each (lambda (state) (enq! queue state)) (next-states state))
      ))
    faces))

(define (parse-instructions input)
  (let loop ((input (string->list input)) (result (list)))
    (match (split-number input) ((step-count (turn . rest)) (loop rest (cons turn (cons step-count result))))
                                ((step-count '()) (reverse (cons step-count result))))))

(define (split-number input)
  (let* ((digits (take-while char-numeric? input)))
    (list (string->number (list->string digits)) (drop input (length digits)))))

(define (read-board) (read-grid (match-lambda (#\space #f) (c c))))

(define (min-x grid y)
  (minimum (map point-x (filter (lambda (p) (= y (point-y p))) (grid-keys grid)))))

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

(define-match* iterate-n
    [(f init 0) '()]
    [(f init count) (cons init (iterate-n f (f init) (1- count)))])

(define (turn-left vec) (step-direction (modulo (- (vec-to-facing vec) 1) 4)))
(define (turn-right vec) (step-direction (modulo (+ (vec-to-facing vec) 1) 4)))

(define (choose-next grid point vec)
  (let* [(left (turn-left vec))
         (right (turn-right vec))
         (ahead (point+ point vec))
         (inside-left (point+ ahead left))
         (outside-right (point+ point right))]
    (cond
      ((hash-ref grid inside-left) (cons inside-left left))
      ((hash-ref grid ahead) (cons ahead vec))
      ((hash-ref grid outside-right) (cons point right)))))

(define (follow-boundary grid start vec n)
  (if (= n 0)
    '()
    (let* ((first-edge (iterate-n (lambda (p) (point+ p vec)) start (cube-size grid)))
           (next (choose-next grid (last first-edge) vec))
           (next-start (car next))
           (next-vec (cdr next)))
      (cons (map (partial list (turn-right vec)) first-edge) (follow-boundary grid next-start next-vec (- n 1))))))

(define (assemble matching-function boundary-segments)
  (pipe>
    (pair-by matching-function boundary-segments)
    (append-map (lambda (parts) (map list (first parts) (reverse (second parts)))))))

(define (point-back point) (make-point (- (point-x point)) (- (point-y point))))

(define (create-boundary boundary)
  (let* ((bounds (make-hash-table))
         (add-bound! (lambda (p1 v1 p2 v2) (hash-set! bounds (make-position p1 (vec-to-facing v1)) (make-position p2 (vec-to-facing v2))))))
    (map (match-lambda (((v1 p1) (v2 p2)) (add-bound! p1 (point-back v1) p2 v2)
                                          (add-bound! p2 (point-back v2) p1 v1))) boundary)
    bounds))

(define (cube-size grid) (sqrt (/ (hash-count (lambda (k v) #t) grid) 6)))

(define-match* cross-product
  ((($ <point3> x1 y1 z1) ($ <point3> x2 y2 z2)) (make-point3 (- (* y1 z2) (* y2 z1)) (- (* z1 x2) (* z2 x1)) (- (* x1 y2) (* x2 y1)))))
