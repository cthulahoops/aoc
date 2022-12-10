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

(define (apply-instruction ins vm)
  ; (display ins)
  ; (display "  ")
  ; (display vm)
  ; (newline)
  (make-vm
    (+ ins (vm-register vm))
    (cons (vm-register vm) (vm-history vm))))

(define (pair-* pair) (* (car pair) (cdr pair)))

(define (compute-result enumerated)
  ; (display enumerated)
  ; (newline)
  (let ((relevant (map (lambda (step) (assq step enumerated)) (list 20 60 100 140 180 220))))
    (sum (map pair-* relevant))))

(define (position->lit pixel-position sprite-position) (if (and (>= pixel-position (- sprite-position 1)) (<= pixel-position (+ sprite-position 1))) #\# #\space))
(define (cycle->column cycle) (modulo (- cycle 1) 40))
(define (visible enumerated) (map (lambda (pair) (position->lit (cycle->column (car pair)) (cdr pair))) enumerated))

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

(define (format-image lines) (string-append "\n" (string-join (map list->string lines) "\n") "\n"))

(define (part2)
  (let* ((instructions (apply append (map parse-instruction (read-lines))))
         (init-vm (make-vm 1 (list))))
    (pipe> (fold apply-instruction init-vm instructions)
           (vm-history)
           (reverse)
           (enumerate)
           (visible)
           (chunk 40)
           (format-image)
           )))
