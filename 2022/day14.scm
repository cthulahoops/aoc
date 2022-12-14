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
      p1)))

(define (expand-path path) (append (append-map expand-segment path (cdr path)) (list (last path))))

(define (make-map rocks)
  (let ((map (make-hash-table)))
    (for-each (lambda (rock) (hash-set! map rock #\#)) rocks)
    map
    ))

(define (down point) (point+ point (make-point 0 1)))
(define (down-left point) (point+ point (make-point -1 1)))
(define (down-right point) (point+ point (make-point 1 1)))

(define (simulate-sand map void start)
  (let ((empty? (lambda (position) (not (hash-ref map position)))))
    (let loop ((point start))
      (cond
        ((> (point-y point) void) #f)
        ((empty? (down point)) (loop (down point)))
        ((empty? (down-left point)) (loop (down-left point)))
        ((empty? (down-right point)) (loop (down-right point)))
        (else point)
      ))))

(define (run-simulation map void)
  (let loop ((n 0))
    (let ((resting-point (simulate-sand map void (make-point 500 0))))
      (if
        resting-point
        (begin
          (hash-set! map resting-point #\o)
          (loop (+ n 1)))
        n))))

(define (part1)
  (let* ((paths (read-cave-system))
         (rocks (apply append (map expand-path paths)))
         (void (maximum (map point-y rocks)))
         (map (make-map rocks)))
    (display void)
    (newline)
    (newline)
    (display (hash-map->list (lambda (x y) x) map))
    (newline)
    (run-simulation map void)
    ))

(define (part2) 0)
