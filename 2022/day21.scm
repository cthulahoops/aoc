(add-to-load-path ".")
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (aoc))

(define (parse-expression parts)
  (match parts ((_space x) (string->number x))
               ((_space a op b) (list (string->symbol op) (string->symbol a) (string->symbol b)))))

(define (parse-line line)
  (match (string-split line #\:)
         ((name expr) (list (string->symbol name) (parse-expression (string-split expr #\space))))))

(define (define-expressions definitions)
  (let ((env (make-hash-table)))
    (for-each (lambda (x) (hash-set! env (first x) (second x))) definitions)
    env))

(define (eval-call operator arg1 arg2)
  (cond ((and (number? arg1) (number? arg2)) (eval-simple operator arg1 arg2))
        (else (list operator arg1 arg2))))

(define (resolve symbol env)
  (match (hash-ref env symbol)
    (#f symbol)
    ((? number? x) x)
    ((op a b) (eval-call op (resolve a env) (resolve b env)))))

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
  (match left ((? number? _) (balance right left))
              ((? symbol? _) (list left right))
              (('/ (? number? a) b) (balance b (/ a right)))
              (('- (? number? a) b) (balance b (- a right)))
              ((op (? number? a) b) (balance b (eval-simple (flip-op op) right a)))
              ((op a (? number? b)) (balance a (eval-simple (flip-op op) right b)))
              ))

(define (part1)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (resolve 'root env)))

(define (part2)
  (let* ((env (define-expressions (map parse-line (read-lines)))))
    (hash-remove! env 'humn)
    (let ((root (hash-ref env 'root))) (hash-set! env 'root (cons '= (cdr root))))
    (second (apply balance (cdr (resolve 'root env))))))
