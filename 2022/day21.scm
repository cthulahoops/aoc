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

(define (transpile lines)
  (append (map parse-line lines) (list '(display (root)) '(newline))))

; (try-eval)
(define (define-expressions definitions)
  (let ((env (make-hash-table))) (for-each (lambda (x) (hash-set! env (first x) (second x))) definitions) env))

(define (eval-call operator arg1 arg2)
  (cond ((and (number? arg1) (number? arg2)) (eval (list operator arg1 arg2) (interaction-environment)))
        (else (list operator arg1 arg2))))

(define (resolve symbol env)
  (let ((expr (hash-ref env symbol)))
    (cond ((equal? expr #f) symbol)
          ((number? expr) expr)
          ((pair? expr) (eval-call (first expr) (resolve (second expr) env) (resolve (third expr) env))))))

(define (part1)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (resolve 'root env)))

(define (part2)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (hash-remove! env 'humn)
    (let ((root (hash-ref env 'root))) (hash-set! env 'root (cons '= (cdr root))))
    (resolve 'root env)))
; (define (part2)
;   (display-lines (map parse-line (read-lines)))
;   0)
