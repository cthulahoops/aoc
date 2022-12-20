(add-to-load-path ".")
(use-modules (aoc))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-1))

(define-immutable-record-type <circular>
  (make-circular offset head tail)
  circular?
  (offset circular-offset)
  (head circular-head)
  (tail circular-tail))

(define (circular lst) (make-circular 0 lst '()))
(define (circular-car circular) (car (circular-head circular)))
(define (circular-cdr circular)
  (if (null? (cdr (circular-head circular)))
        (make-circular 0
                       (reverse (cons (car (circular-head circular)) (circular-tail circular)))
                       (list))
        (make-circular (1+ (circular-offset circular))
                       (cdr (circular-head circular))
                       (cons (car (circular-head circular)) (circular-tail circular)))))

(define (circular-pop-cdr circular)
  (if (null? (cdr (circular-head circular)))
        (make-circular 0 (reverse (circular-tail circular)) (list))
        (make-circular (circular-offset circular) (cdr (circular-head circular)) (circular-tail circular))))

(define (circular-scan circular value)
  (if (equal? (circular-car circular) value)
      circular
      (circular-scan (circular-cdr circular) value)))

(define (circular-scan-to-offset circular offset)
  (if (= offset (circular-offset circular))
      circular
      (circular-scan-to-offset (circular-cdr circular) offset)))

(define (circular-insert circular value)
  (make-circular (circular-offset circular) (cons value (circular-head circular)) (circular-tail circular)))

(define (mix value circular)
  (let ((source-position (circular-scan circular value)))
    (circular-insert (circular-scan-to-offset (circular-pop-cdr source-position) (modulo (+ (circular-offset source-position) (cdr value)) 4999)) value)))


(define (circular->list circular) (append (circular-head circular) (reverse (circular-tail circular))))

(define (part1)
  (let* ((input (enumerate (map string->number (read-lines))))
         (input-size (length input))
         (mixed (map cdr (circular->list (fold mix (circular input) input))))
         (zero-position (list-index (lambda (x) (= x 0)) mixed))
        )
    (display input-size)
    (newline)
    (+ (list-ref mixed (modulo (+ zero-position 1000) input-size))
       (list-ref mixed (modulo (+ zero-position 2000) input-size))
       (list-ref mixed (modulo (+ zero-position 3000) input-size)))
    ))
(define (part2) 0)
