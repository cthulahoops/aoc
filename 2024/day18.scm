(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 q))  ; For priority queue

;; Point type
(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))

(define (point+ p1 p2) 
  (make-point (+ (point-x p1) (point-x p2)) 
              (+ (point-y p1) (point-y p2))))

;; Grid data type
(define-record-type <grid-data>
  (make-grid-data width height table)
  grid-data?
  (width grid-width)
  (height grid-height)
  (table grid-table))

;; Function to split a string by comma
(define (split-by-comma str)
  (let loop ((chars (string->list str))
             (current '())
             (result '()))
    (cond
      ((null? chars)
       (reverse (cons (list->string (reverse current)) result)))
      ((char=? (car chars) #\,)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result)))
      (else
       (loop (cdr chars)
             (cons (car chars) current)
             result)))))

;; Parse coordinate line to point
(define (parse-coordinate-line line)
  (let* ((parts (split-by-comma (string-trim-both line)))
         (x (string->number (car parts)))
         (y (string->number (cadr parts))))
    (make-point x y)))

;; Coordinate table functions
(define (make-coordinate-table)
  (make-hash-table))

(define (coordinate-table-add! table point line-num)
  (if (not (hash-ref table point))
      (hash-set! table point line-num)))

(define (coordinate-table-get-line table point)
  (hash-ref table point #f))

(define (coordinate-table->alist table)
  (hash-map->list cons table))

(define (coordinate-table-reverse-lookup table line-num)
  (let ((alist (coordinate-table->alist table)))
    (let loop ((alist alist))
      (if (null? alist)
          #f
          (let ((pair (car alist)))
            (if (= (cdr pair) line-num)
              (car pair)
              (loop (cdr alist))))))))

;; Read coordinates from file
(define (read-coordinates filename width height)
  (let* ((port (open-input-file filename))
         (table (make-coordinate-table)))
    (let loop ((line (read-line port))
               (line-num 1))
      (if (eof-object? line)
          (begin
            (close-port port)
            (make-grid-data width height table))
          (begin
            (coordinate-table-add! 
              table 
              (parse-coordinate-line line) 
              line-num)
            (loop (read-line port) (+ line-num 1)))))))

;; Lookup function
(define (lookup grid-data x y t)
  (let* ((width (grid-width grid-data))
         (height (grid-height grid-data))
         (table (grid-table grid-data))
         (point (make-point x y)))
    (or (< x 0)
        (> x width)
        (< y 0)
        (> y height)
        (let ((line-num (coordinate-table-get-line table point)))
          (and line-num (<= line-num t))))))

;; Manhattan distance heuristic
(define (manhattan-distance p1 p2)
  (+ (abs (- (point-x p2) (point-x p1)))
     (abs (- (point-y p2) (point-y p1)))))

;; Get valid neighbors
(define (get-neighbors point)
  (list (point+ point (make-point 0 1))   ; up
        (point+ point (make-point 1 0))   ; right
        (point+ point (make-point 0 -1))  ; down
        (point+ point (make-point -1 0)))) ; left


(define-record-type <priority-queue>
  (%make-priority-queue vec size)
  priority-queue?
  (vec priority-queue-vec set-priority-queue-vec!)
  (size priority-queue-size set-priority-queue-size!))

(define (make-priority-queue)
  (%make-priority-queue (make-vector 32) 0))

(define (parent idx) (quotient (- idx 1) 2))
(define (left-child idx) (+ (* idx 2) 1))
(define (right-child idx) (+ (* idx 2) 2))

(define (vec-swap! vec i j)
  (let ((temp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j temp)))

(define (ensure-capacity! pq)
  (let ((vec (priority-queue-vec pq)))
    (when (>= (priority-queue-size pq) (vector-length vec))
      (let ((new-vec (make-vector (* 2 (vector-length vec)))))
        (let loop ((i 0))
          (when (< i (vector-length vec))
            (vector-set! new-vec i (vector-ref vec i))
            (loop (+ i 1))))
        (set-priority-queue-vec! pq new-vec)))))

(define (priority-queue-insert! pq item priority)
  (ensure-capacity! pq)
  (let ((vec (priority-queue-vec pq))
        (size (priority-queue-size pq)))
    (vector-set! vec size (cons priority item))
    (set-priority-queue-size! pq (+ size 1))
    (let up ((idx size))
      (let ((p (parent idx)))
        (when (and (> idx 0)
                  (> (car (vector-ref vec idx))
                     (car (vector-ref vec p))))
          (vec-swap! vec idx p)
          (up p))))))

(define (priority-queue-pop! pq)
  (if (= 0 (priority-queue-size pq))
      #f
      (let* ((vec (priority-queue-vec pq))
             (last-idx (- (priority-queue-size pq) 1))
             (result (cdr (vector-ref vec 0))))
        (vector-set! vec 0 (vector-ref vec last-idx))
        (set-priority-queue-size! pq last-idx)
        (let down ((idx 0))
          (let* ((left (left-child idx))
                 (right (right-child idx)))
            (when (< left last-idx)
              (let ((largest (if (and (< right last-idx)
                                    (> (car (vector-ref vec right))
                                       (car (vector-ref vec left))))
                               right
                               left)))
                (when (> (car (vector-ref vec largest))
                        (car (vector-ref vec idx)))
                  (vec-swap! vec idx largest)
                  (down largest))))))
        result)))


;; Find path function
(define (find-path grid-data upto)
  (let* ((width (grid-width grid-data))
         (height (grid-height grid-data))
         (start (make-point 0 0))
         (goal (make-point width height))
         (open-set (make-priority-queue))
         (came-from (make-hash-table))
         (g-score (make-hash-table))
         (f-score (make-hash-table)))

    ; Initialize start node
    (hash-set! g-score start 0)
    (hash-set! f-score start (manhattan-distance start goal))
    (priority-queue-insert! open-set start (manhattan-distance start goal))
    
    (let loop ()
      (let ((current (priority-queue-pop! open-set)))
        (cond
         ((not current) #f)  ; No path found
         ((equal? current goal)
          ; Reconstruct path
          (let path-loop ((current current)
                         (path '()))
            (if (equal? current start)
                path
                (path-loop (hash-ref came-from current)
                          (cons current path)))))
         (else
          ; Process neighbors
          (for-each
           (lambda (neighbor)
             (let ((tentative-g-score 
                    (+ (hash-ref g-score current 1000000)
                       1)))
               (when (and (not (lookup grid-data 
                                     (point-x neighbor)
                                     (point-y neighbor)
                                     upto))
                         (or (not (hash-ref g-score neighbor #f))
                             (< tentative-g-score 
                                (hash-ref g-score neighbor))))
                 (hash-set! came-from neighbor current)
                 (hash-set! g-score neighbor tentative-g-score)
                 (hash-set! f-score neighbor
                           (+ tentative-g-score
                              (manhattan-distance neighbor goal)))
                 (priority-queue-insert! open-set neighbor
                                       (* -1 (hash-ref f-score neighbor)))
               )))
           (get-neighbors current))
          (loop)))))))

(define (part2 grid) 
  (bisect (lambda (x) (find-path grid x)) 0 4096))

(define (bisect f low high)
  (display "Trying: ")
  (display low)
  (display ",")
  (display high)
  (newline)
  (if (= low high)
      low
      (let ((mid (+ (quotient (+ low high) 2) 1)))
        (if (f mid)
            (bisect f mid high)
            (bisect f low (- mid 1))))))

(define (run-program grid cutoff)
    (display "Part 1: ")
    (display (length (find-path grid cutoff)))
    (newline)
    (display "Part 2: ")
    (display (coordinate-table-reverse-lookup (grid-table grid) (+ (part2 grid) 1)))
    (newline)
    (newline))

(define (main)
  (run-program (read-coordinates "input/18" 70 70) 1024))

(define (example)
  (run-program (read-coordinates "input/18-example" 6 6) 12))
