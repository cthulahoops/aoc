(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (aoc))

(define (parse-symbol symbol) (string->symbol symbol))
(define (parse-expression parts)
  (match (length parts) (2 (string->number (second parts)))
                        (4 (list (string->symbol (third parts)) (string->symbol (second parts)) (string->symbol (fourth parts))))))

(define (parse-line line)
  (let ((split (string-split line #\:)))
    (list (parse-symbol (first split)) (parse-expression (string-split (second split) #\space)))))

(define (define-expressions definitions)
  (let ((env (make-hash-table))) (for-each (lambda (x) (hash-set! env (first x) (second x))) definitions) env))

(define (eval-call operator arg1 arg2)
  (cond ((and (number? arg1) (number? arg2)) (eval-simple operator arg1 arg2))
        (else (list operator arg1 arg2))))

(define (resolve symbol env)
  (let ((expr (hash-ref env symbol)))
    (cond ((equal? expr #f) symbol)
          ((number? expr) expr)
          ((pair? expr) (eval-call (first expr) (resolve (second expr) env) (resolve (third expr) env))))))

(define (flip-op op)
  (match op ('/ '*)
            ('* '/)
            ('- '+)
            ('+ '-)))

(define (op x)
  (match x ('/ /)
           ('* *)
           ('+ +)
           ('- -)))


(define (eval-simple symbol arg1 arg2) ((op symbol) arg1 arg2))

(define (balance left right)
  ; (display left)
  ; (display '=)
  ; (display right)
  ; (newline)
  (cond ((number? left) (balance right left))
        ((symbol? left) (list left right))
        ((and (equal? '/ (first left)) (number? (second left))) (balance (third left) (/ (second left) right)))
        ((and (equal? '- (first left)) (number? (second left))) (balance (third left) (- (second left) right)))
        ((number? (second left)) (balance (third left) (eval-simple (flip-op (first left)) right (second left))))
        ((number? (third left)) (balance (second left) (eval-simple (flip-op (first left)) right (third left))))))

(define (part1)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (resolve 'root env)))

(define (part2)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (hash-remove! env 'humn)
    (let ((root (hash-ref env 'root))) (hash-set! env 'root (cons '= (cdr root))))
    (second (apply balance (cdr (resolve 'root env))))))
