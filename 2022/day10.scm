(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))

(define (parse-instruction line)
  (match (string-split line #\space) (("noop") (list 0))
                                     (("addx" x) (list 0 (string->number x)))))

(define (iterate f init items)
  (fold (lambda (item acc) (cons (f item (car acc)) acc)) (list init) items))

(define (pair-* pair) (* (car pair) (cdr pair)))

(define (compute-result enumerated)
  (let ((relevant (map (lambda (step) (assq step enumerated)) (list 20 60 100 140 180 220))))
    (sum (map pair-* relevant))))

(define (position->lit pixel-position sprite-position) (if (<= (abs (- pixel-position sprite-position)) 1) #\# #\space))
(define (cycle->column cycle) (modulo (- cycle 1) 40))
(define (visible enumerated) (map (lambda (pair) (position->lit (cycle->column (car pair)) (cdr pair))) enumerated))

(define (read-instructions) (apply append (map parse-instruction (read-lines))))

(define (part1)
  (pipe> (read-instructions)
         (iterate + 1)
         (reverse)
         (enumerate)
         (compute-result)
         ))

(define (format-image lines) (string-append "\n" (string-join (map list->string lines) "\n") "\n"))

(define (part2)
  (pipe> (read-instructions)
         (iterate + 1)
         (cdr)
         (reverse)
         (enumerate)
         (visible)
         (chunk 40)
         (format-image)
         ))
