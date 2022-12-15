(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (ice-9 textual-ports))

(define (compare-numbers x y)
  (cond ((< x y) 'lt)
        ((= x y) 'eq)
        ((> x y) 'gt)))

(define (compare x y)
  (cond
    ((and (number? x) (number? y)) (compare-numbers x y))
    ((number? y) (compare x (list y)))
    ((number? x) (compare (list x) y))
    ((and (pair? x) (pair? y))
        (match (compare (car x) (car y)) ('eq (compare (cdr x) (cdr y)))
                                         (otherwise otherwise)))
    ((and (null? x) (null? y)) 'eq)
    ((and (null? x) (pair? y)) 'lt)
    ((and (pair? x) (null? y)) 'gt)))

(define (packet<  x y) (equal? (compare x y) 'lt))
(define (packet<= x y) (not (packet> x y)))
(define (packet=  x y) (equal? (compare x y) 'eq))
(define (packet>= x y) (not (packet< x y)))
(define (packet>  x y) (equal? (compare x y) 'gt))

(define (indices-where f items)
  (pipe>
    (enumerate items)
    (filter (lambda (x) (f (cdr x))))
    (map car)))

(define (part1)
  (let* ((pairs (chunk 2 (gather-list read eof-object?))))
    (sum (indices-where (partial apply packet<) pairs))))

(define count-where (compose length filter))

(define (part2)
  (let* ((packets (append (list '2 '6) (gather-list read eof-object?))))
    (* (count-where (lambda (x) (packet<= x 2)) packets) (count-where (lambda (x) (packet<= x 6)) packets))))
