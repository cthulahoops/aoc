(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (aoc))

(define (hash-items hash) (hash-map->list cons hash))
(define (hash-keys hash) (hash-map->list (lambda (k v) k) hash))

(define (parse-board-line grid) 
  (match-lambda ((y . line)
     (map (match-lambda ((x . #\.) x)
                        ((x . c) (hash-set! grid (make-point x y) #t))) (enumerate (string->list line))))))

(define north (make-point 0 -1))
(define south (make-point 0 1))
(define west (make-point -1 0))
(define east (make-point 1 0))
(define nw (point+ north west))
(define ne (point+ north east))
(define sw (point+ south west))
(define se (point+ south east))

(define (look grid elf directions) (find (lambda (d) (hash-ref grid (point+ elf d))) directions))
(define (look-all grid elf) (look grid elf (list north south west east nw ne sw se)))
(define (look-north grid elf) (look grid elf (list north nw ne)))
(define (look-south grid elf) (look grid elf (list south sw se)))
(define (look-west grid elf) (look grid elf (list west nw sw)))
(define (look-east grid elf) (look grid elf (list east ne se)))

(define (find-clear-move grid elf priorities)
  (match (find (match-lambda ((f d) (not (f grid elf)))) priorities)
          (#f #f)
          ((f d) (point+ d elf))))

(define (proposal grid priorities elf)
  (or (not (look-all grid elf)) (find-clear-move grid elf priorities)))

(define init-priorities
  (list
    (list look-north north)
    (list look-south south)
    (list look-west west)
    (list look-east east)))

(define (parse-board board)
  (let ((grid (make-hash-table)))
    (map (parse-board-line grid) (enumerate board))
    grid))

(define (duplicates lst)
  (let ((counts (make-hash-table)))
    (for-each (lambda (k) (hash-set! counts k (1+ (or (hash-ref counts k) 0)))) lst)
    (map car (filter (lambda (item) (> (cdr item) 1)) (hash-items counts)))))

(define (compute-moves elves priorities)
  (let* ((moves (filter (lambda (x) (point? (cdr x))) (map (lambda (elf) (cons elf (proposal elves priorities elf))) (hash-keys elves))))
         (duplicates (duplicates (filter identity (map cdr moves)))))
;    (display (list 'dups duplicates))
;    (newline)
    (filter cdr (map (lambda (move) (if
                          (member (cdr move) duplicates)
                          (cons (car move) #f)
                          move)) moves))))

(define (apply-move! elves move)
      (hash-remove! elves (car move))
      (hash-set! elves (cdr move) #t))

(define (update-elf-positions! elves priorities)
  (let* ((moves (compute-moves elves priorities)))
    ; (display-lines moves)
    ; (newline)
    (for-each (partial apply-move! elves) moves)
    (length moves)))

(define (new-priorities priorities) (append (cdr priorities) (list (car priorities))))

(define (run-simulation max-rounds elves priorities)
  (let loop ((n 0) (priorities priorities))
    (display (list n))
    (newline)
    (if (and (< n max-rounds) (> (update-elf-positions! elves priorities) 0))
      (begin
;        (display-grid elves)
        (loop (1+ n) (new-priorities priorities)))
      (1+ n))))

(define (box-size hash-table)
  (let* (
         (elves (hash-keys hash-table))
         (x0 (minimum (map point-x elves)))
         (x1 (maximum (map point-x elves)))
         (y0 (minimum (map point-y elves)))
         (y1 (maximum (map point-y elves)))
        )
    (* (1+ (- x1 x0)) (1+ (- y1 y0)))))

(define (display-grid-line hash-table y x0 x1)
  (display (list->string (map (lambda (x) (if (hash-ref hash-table (make-point x y)) #\# #\.)) (range x0 (1+ x1)))))
  (newline))

(define (display-grid hash-table)
  (let* (
         (elves (hash-keys hash-table))
         (x0 (minimum (map point-x elves)))
         (x1 (maximum (map point-x elves)))
         (y0 (minimum (map point-y elves)))
         (y1 (maximum (map point-y elves)))
        )
    (for-each (lambda (y) (display-grid-line hash-table y (min 1 x0) x1)) (range (min 1 y0) (1+ y1)))))

(define (part1)
  (let* ((elves (parse-board (read-block))))
;    (display-grid elves)
    (run-simulation 10 elves init-priorities)
    (- (box-size elves) (length (hash-keys elves)))
    ))

(define (part2)
  (let* ((elves (parse-board (read-block))))
;    (display-grid elves)
    (run-simulation 1000 elves init-priorities)
    ))
