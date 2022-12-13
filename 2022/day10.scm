(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (parse-instruction line)
  (match (string-split line #\space) (("noop") (list 0))
                                     (("addx" x) (list 0 (string->number x)))))

(define (apply-pair f pair) (f (car pair) (cdr pair)))
(define (pair-* pair) (apply-pair * pair))

(define (compute-result enumerated)
  (let ((relevant (map (lambda (step) (assq step enumerated)) (list 20 60 100 140 180 220))))
    (sum (map pair-* relevant))))

(define (position->lit pixel-position sprite-position) (if (<= (abs (- (cycle->column pixel-position) sprite-position)) 1) #\# #\space))
(define (cycle->column cycle) (modulo (- cycle 1) 40))
(define (visible enumerated) (map (lambda (pair) (apply-pair position->lit pair)) enumerated))

(define (read-instructions) (apply append (map parse-instruction (read-lines))))

(define (part1)
  (pipe> (read-instructions)
         (iterate + 1)
         (enumerate)
         (compute-result)
         ))

(define (format-image lines) (string-append "\n" (string-join (map list->string lines) "\n") "\n"))

(define (part2)
  (pipe> (read-instructions)
         (iterate + 1)
         ((flip take) (* 6 40))
         (enumerate)
         (visible)
         (chunk 40)
         (format-image)
         ))
