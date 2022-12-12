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

(define (climbable height1 height2)
  (<= (- height2 height1) 1))

(define (reachable grid point rule)
  (let ((neighbours (neighbours point))
        (height (hash-ref grid point)))
    (filter (lambda (neighbour) (let ((neighbour-height (hash-ref grid neighbour))) (and neighbour-height (rule height neighbour-height)))) neighbours)))

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

(define (search next done? start)
  (let loop ((queue (enq! (make-q) (cons start 0)))
             (visited (make-hash-table)))
    (if
      (q-empty? queue)
      #f
      (let* ((item (q-pop! queue))
             (current (car item))
             (distance (cdr item)))
        (if
          (hash-ref visited current)
          (loop queue visited)
          (begin
            (hash-set! visited current distance)
            (if
              (done? current)
              distance
              (begin
                (map (lambda (item) (enq! queue (cons item (+ distance 1)))) (next current))
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
        (next (lambda (current) (reachable grid current climbable)))
        (done? (lambda (current) (equal? current end)))
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
    (search next done? start)))

(define (part2)
  (let* ((grid-points (read-grid))
        (start (find-start grid-points))
        (end (find-end grid-points))
        (grid (alist->hash-table (map-cdr height grid-points)))
        (next (lambda (current) (reachable grid current (flip climbable))))
        (done? (lambda (current) (= (hash-ref grid current) (char->integer #\a))))
        )
    (search next done? end)))
