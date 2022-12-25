(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (part1)
    (let* [(input (read-lines))]
      (number->snafu (sum (map snafu->number input)))))

(define (part2) 0)

(define (snafu->number x) (add-digits (map digit->number (string->list x))))

(define (number->snafu x)
  (list->string (map number->snafu-digit (reverse (snafu-digits x)))))


(define (snafu-digits x)
  (let loop [(x x) (base (highest-base x)) (result (list))]
    (if (< base 1)
        result
        (let* [(digit (base-digit base x))
               (amount (begin (display digit) (newline) (* digit base)))]
          (loop (- x amount) (/ base 5) (cons digit result))
          )
    )))

(define (highest-base x) (find (lambda (y) (< (abs x) (* 3 y))) (map (partial expt 5) (range 0 50))))

(define (base-digit base x)
   (minimum-by (lambda (d) (abs (- x (* d base)))) (list -2 -1 0 1 2)))

(define (digit->number x)
  (match x (#\= -2) (#\- -1) (#\0 0) (#\1 1) (#\2 2)))

(define (number->snafu-digit x)
  (match x (-2 #\=) (-1 #\-) (0 #\0) (1 #\1) (2 #\2)))

(define (add-digits digits) (sum (map (match-lambda ((n . x) (* x (expt 5 (- n 1))))) (enumerate (reverse digits)))))
