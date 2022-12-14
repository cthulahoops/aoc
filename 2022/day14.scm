(add-to-load-path ".")
(use-modules (aoc))
(use-modules (srfi srfi-1))

(define (parse-item item)
  (pipe>
      (string-trim-both item (char-set-complement char-set:digit))
      ((flip string-split) #\,)
      (map string->number)
      (apply make-point)))

(define (parse-line line) (map parse-item (string-split line #\>)))
(define (read-cave-system)
  (map parse-line (read-lines)))

(define (expand-segment p1 p2)
  (let ((step (point-sign (point- p2 p1))))
    (unfold
      (lambda (x) (equal? p2 x))
      (lambda (x) x)
      (lambda (x) (point+ x step))
      p1
      (lambda (x) (list x)))))

(define (expand-path path) (append-map expand-segment path (cdr path)))

(define (make-map rocks)
  (let ((map (make-hash-table)))
    (for-each (lambda (rock) (hash-set! map rock #\#)) rocks)
    map
    ))

(define (const value) (lambda (x) value))

(define (down point) (point+ point (make-point 0 1)))
(define (down-left point) (point+ point (make-point -1 1)))
(define (down-right point) (point+ point (make-point 1 1)))

(define (simulate-sand hash-table done? onfloor? start)
  (let ((empty? (lambda (position) (not (hash-ref hash-table position)))))
    (let loop ((point start))
      (cond
        ((done? point) #f)
        ((onfloor? point) point)
        ((empty? (down point)) (loop (down point)))
        ((empty? (down-left point)) (loop (down-left point)))
        ((empty? (down-right point)) (loop (down-right point)))
        (else point)
      ))))

(define (run-simulation hash-table start-point done? onfloor?)
  (let loop ((n 0))
    (let ((resting-point (simulate-sand hash-table done? onfloor? start-point)))
      (cond
        ((not resting-point) n)
        ((equal? resting-point start-point) (+ n 1))
        (else
          (begin
            (hash-set! hash-table resting-point #\o)
            (loop (+ n 1)))
          )))))

(define (part1)
  (let* ((paths (read-cave-system))
         (rocks (apply append (map expand-path paths)))
         (start-point (make-point 500 0))
         (void (maximum (map point-y rocks)))
         (done? (lambda (point) (> (point-y point) void)))
         (hash-table (make-map rocks)))
    (run-simulation hash-table start-point done? (const #f))
    ))

(define (part2)
  (let* ((paths (read-cave-system))
         (rocks (apply append (map expand-path paths)))
         (start-point (make-point 500 0))
         (floor (+ (maximum (map point-y rocks))))
         (onfloor? (lambda (point) (= (point-y point) (+ floor 1))))
         (hash-table (make-map rocks)))
    (run-simulation hash-table start-point (const #f) onfloor?)
    ))
