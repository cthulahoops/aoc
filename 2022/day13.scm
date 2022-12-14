(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define (compare-numbers x y)
  (cond ((< x y) 'good)
        ((= x y) 'maybe)
        ((> x y) 'bad)))

(define (compare x y)
  ; (display (list x y))
  ; (newline)
  (cond
    ((and (number? x) (number? y)) (compare-numbers x y))
    ((number? y) (compare x (list y)))
    ((number? x) (compare (list x) y))
    ((and (pair? x) (pair? y))
        (match (compare (car x) (car y)) ('good 'good)
                                         ('bad 'bad)
                                         ('maybe (compare (cdr x) (cdr y)))))
    ((and (null? x) (null? y)) 'maybe)
    ((and (null? x) (pair? y)) 'good)
    ((and (pair? x) (null? y)) 'bad)
    ))

(define (index-sum-where f items)
  (pipe>
    (enumerate items)
    (filter (lambda (x) (f (cdr x))))
    (map car)
    (sum)))

(define (list-index items x)
  (let loop ((n 1) (items items))
    (if
      (equal? (car items) x)
      n
      (loop (+ n 1) (cdr items)))))

(define (packet< x y) (equal? (compare x y) 'good))

(define (part1)
  (let* ((pairs (chunk 2 (gather-list read eof-object?))))
    (index-sum-where (partial apply packet<) pairs)))

(define (count-smaller-than special packets)
  (length (filter (lambda (x) (packet< x special)) packets)))

(define (part2)
  (let* ((packets (gather-list read eof-object?)))
    (* (+ (count-smaller-than 2 packets) 1) (+ (count-smaller-than 6 packets) 2))))
