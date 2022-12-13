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
    ((and (pair? x) (pair? y)) (match (compare (car x) (car y)) ('good 'good)
                                                                ('bad 'bad)
                                                                ('maybe (compare (cdr x) (cdr y)))))
    ((and (null? x) (null? y)) 'maybe)
    ((and (null? x) (pair? y)) 'good)
    ((and (pair? x) (null? y)) 'bad)
    ))

(define (index-sum items) (sum (map car (filter (lambda (x) (equal? (cdr x) 'good)) (enumerate items)))))

(define (list-index x items)
  (if (equal? (car items) x) 1 (+ 1 (list-index x (cdr items)))))

(define (part1)
  (let* ((pairs (chunk 2 (gather-list read eof-object?))))
  ; (let* ((pairs (map (lambda (x) (map read x)) (read-blocks))))
    (index-sum (map (lambda (pair) (apply compare pair)) pairs))))

(define magic-packets (list '((2)) '((6))))
(define (part2)
  (let* ((packets (gather-list read eof-object?))
         (packets (append magic-packets packets))
         (sorted (sort packets (lambda (x y) (equal? (compare x y) 'good)))))
    (apply * (map (lambda (packet) (list-index packet sorted)) magic-packets))))
