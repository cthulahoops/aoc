(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <vm>
  (make-vm register history)
  vm?
  (register vm-register)
  (history vm-history))

(define (parse-instruction line)
  (match (string-split line #\space) (("noop") (list 0))
                                     (("addx" x) (list 0 (string->number x)))))

(define (enumerate items)
  (let loop ((count 1) (items items) (result (list)))
    (if (null? items)
        (reverse result)
        (loop (+ 1 count) (cdr items) (cons (cons count (car items)) result)))))

(define (apply-instruction ins vm)
  (display ins)
  (display "  ")
  (display vm)
  (newline)
  (make-vm
    (+ ins (vm-register vm))
    (cons (vm-register vm) (vm-history vm))))

(define (pair-* pair) (* (car pair) (cdr pair)))

(define (compute-result enumerated)
  (display enumerated)
  (newline)
  (let ((relevant (map (lambda (step) (assq step enumerated)) (list 20 60 100 140 180 220))))
    (display relevant)
    (newline)
    (sum (map pair-* relevant))))

(define (part1)
  (let* ((instructions (apply append (map parse-instruction (read-lines))))
         (init-vm (make-vm 1 (list))))
    ; (display instructions)
    ; (newline)
    (pipe> (fold apply-instruction init-vm instructions)
           (vm-history)
           (reverse)
           (enumerate)
           (compute-result)
           )))
(define (part2) 0)
