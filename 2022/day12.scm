(add-to-load-path ".")
(use-modules (aoc))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 match))
(use-modules (ice-9 q))

(define (read-grid) (apply append (map make-points (enumerate (map parse-line (read-lines))))))
(define (parse-line line) (enumerate (string->list line)))

(define (make-points line)
  (let ((row (car line))
        (data (cdr line)))
    (map (lambda (item) (cons (make-point (car item) row) (cdr item))) data)))

(define (reachable grid point)
  (let ((neighbours (neighbours point))
        (height (hash-ref grid point)))
    (filter (lambda (neighbour) (let ((neighbour-height (hash-ref grid neighbour))) (and neighbour-height (<= (- neighbour-height height) 1)))) neighbours)))

(define (neighbours point)
  (list (point-+ point (make-point 1 0))
        (point-+ point (make-point -1 0))
        (point-+ point (make-point 0 1))
        (point-+ point (make-point 0 -1))))

(define (difference x y) (abs (- x y)))

(define (map-cdr f pairs) (map (lambda (pair) (cons (car pair) (f (cdr pair)))) pairs))
(define (height letter)
  (match letter (#\S (height #\a))
                (#\E (height #\z))
                (letter (char->integer letter))))

(define (search grid start end)
  (let loop ((queue (enq! (make-q) (cons start 0)))
             (visited (make-hash-table)))
    (if
      (q-empty? queue)
      10000
      (let* ((item (q-pop! queue))
             (current (car item))
             (distance (cdr item)))
        (if
          (hash-ref visited current)
          (loop queue visited)
          (begin
            (hash-set! visited current distance)
            (if
              (equal? current end)
              distance
              (begin
                (map (lambda (item) (enq! queue (cons item (+ distance 1)))) (reachable grid current))
                (loop queue visited)))))))))

(define (find-start point-list) (find #\S point-list))
(define (find-end point-list) (find #\E point-list))


(define (find-starts point-list) (map car (filter (lambda (pair) (equal? (cdr pair) #\a)) point-list)))

(define (find value point-list)
  (if (null? point-list)
    #f
    (let ((first-key (car (car point-list)))
          (first-value (cdr (car point-list))))
      (if (equal? first-value value)
        first-key
        (find value (cdr point-list))))))


(define (part1)
  (let* ((grid-points (read-grid))
        (start (find-start grid-points))
        (end (find-end grid-points))
        (grid (alist->hash-table (map-cdr height grid-points)))
        (result (search grid start end))
        )
    (format #t "Start: ~s\n" start)
    (format #t "End: ~s\n" end)
    (newline)
    ; (pipe>
    ;   (map (lambda (pair) (if (hash-ref result (car pair)) (cdr pair) (char-upcase (cdr pair)))) grid-points)
    ;   (chunk 143)
    ;   (map list->string)
    ;   (display-lines)
    ;   (length)
    ;   )
    result
    ))

(define (part2)
  (let* ((grid-points (read-grid))
        (starts (find-starts grid-points))
        (end (find-end grid-points))
        (grid (alist->hash-table (map-cdr height grid-points)))
        ; (result )
        )
    (format #t "Starts: ~s\n" (length starts))
    (format #t "End: ~s\n" end)
    (minimum (map (lambda (start) (search grid start end)) starts))))
