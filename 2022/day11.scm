(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 regex))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-13))

(define-record-type <monkey>
  (make-monkey id valuables operation test next-true next-false trail)
  monkey?
  (id monkey-id)
  (valuables monkey-valuables)
  (operation monkey-operation)
  (test monkey-test)
  (next-true monkey-next-true)
  (next-false monkey-next-false)
  (trail monkey-trail))

(define (string-first-group regex string) (match:substring (string-match regex string) 1))

(define (parse-operation-element element)
  (cond ((string->number element) (string->number element))
        (else (string->symbol element))))

(define (parse-operation operation)
  (let ((elements (map parse-operation-element (string-split operation #\space))))
    (eval (list 'lambda (list 'old) (list (cadr elements) (car elements) (caddr elements))) (interaction-environment))))

(define (trim s) (substring s (string-index s char-numeric?)))

(define (read-monkey)
  (let ((first-line (read-line)))
    (if (eof-object? first-line)
        first-line
        (make-monkey
          (string->number (string-first-group "Monkey ([0-9]+):" first-line))
          (map (compose string->number trim) (string-split (string-first-group " *Starting items: ([0-9, ]+)" (read-line)) #\,))
          (parse-operation (string-first-group " *Operation: new = (.*)" (read-line)))
          (string->number (string-first-group " Test: divisible by ([0-9]+)" (read-line)))
          (string->number (string-first-group " If true: throw to monkey ([0-9])" (read-line)))
          (string->number (string-first-group " If false: throw to monkey ([0-9])" (read-line)))
          (read-line)
        ))))

(define (alist->hash-table alist)
  (let ((table (make-hash-table)))
    (map (lambda (pair) (hash-set! table (car pair) (cdr pair))) alist)
    table
    ))

(define (throw-to! table monkey valuable)
  (hash-set! table monkey (cons valuable (hash-ref table monkey))))

(define (apply-monkey-business! calming table monkey valuable)
  (let* ((played-with ((monkey-operation monkey) valuable))
         (calmed (calming played-with))
         (next-monkey (if (= (modulo calmed (monkey-test monkey)) 0) (monkey-next-true monkey) (monkey-next-false monkey))))
    (throw-to! table next-monkey calmed)))

(define (run-monkey! calming table monkey)
  (let ((valuables (reverse (hash-ref table (monkey-id monkey)))))
    (hash-set! table (monkey-id monkey) (list))
    (map (lambda (valuable) (apply-monkey-business! calming table monkey valuable)) valuables)
    (length valuables)))

(define (run-monkey-round! calming table monkeys)
  (map (lambda (monkey) (run-monkey! calming table monkey)) monkeys))

(define (compute-monkey-business inspections)
  (apply * (take (sort inspections >) 2)))

(define (range n m)
  (let loop ((n n) (result (list)))
    (if
      (> n m)
      (reverse result)
      (loop (+ n 1) (cons n result)))))

(define (add-result new old)
  (if (null? old) new (map-in-order + new old)))

(define (main-loop f n)
  (let loop ((i 0) (result '()))
    (if (>= i n)
        result
        (loop (+ i 1) (add-result (f) result)))))

(define (read-monkeys) (gather-list read-monkey eof-object?))
(define (part1)
  (let* ((monkeys (read-monkeys))
         (init_valuables (map (lambda (monkey) (cons (monkey-id monkey) (reverse (monkey-valuables monkey)))) monkeys))
         (valuable-table (alist->hash-table init_valuables))
         (calming (lambda (value) (floor (/ value 3)))))
    (pipe>
      (main-loop (lambda () (run-monkey-round! calming valuable-table monkeys)) 20)
      (compute-monkey-business))))

(define (part2)
  (let* ((monkeys (read-monkeys))
         (init_valuables (map (lambda (monkey) (cons (monkey-id monkey) (reverse (monkey-valuables monkey)))) monkeys))
         (valuable-table (alist->hash-table init_valuables))
         (calming-factor (apply * (map monkey-test monkeys)))
         (calming (lambda (value) (modulo value calming-factor))))
    (pipe>
      (main-loop (lambda () (run-monkey-round! calming valuable-table monkeys)) 10000)
      (compute-monkey-business))))
