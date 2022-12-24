(add-to-load-path ".")
(use-modules (aoc))
(use-modules (grid))
(use-modules (pfds psqs))
(use-modules (srfi srfi-1))
(use-modules (ice-9 receive))
(use-modules (ice-9 match))

(define (part1)
  (let* (
    (grid (read-board))
    (bounds (grid-bounds grid))
    (blizzards (extract-blizzards grid))
    (blizzard-history (make-hash-table)))

  (display bounds)
  (newline)
  (display (start-point grid))
  (newline)
  (display (end-point grid))
  (newline)
  (clear-blizzards! grid)

  (do
    ((t 0 (1+ t)) (blizzards blizzards (update-blizzards bounds blizzards)))
    ((> t 500))
    ; (display (list t blizzards))
    ; (newline)
    ; (clear-blizzards! grid)
    ; (add-blizzards! grid blizzards)
    ; (display-grid identity grid)
    (for-each (lambda (b) (hash-set! blizzard-history (cons t (car b)) #t)) blizzards)
    ; (newline)
  )

 ; (display (grid-keys blizzard-history))
; (newline)
 ; (newline)

   (a-star (start-point grid) (end-point grid) (next-points grid blizzard-history) manhatten-distance)
 ))

(define (part2) 0)

(define (a-star start-point end-point next-states cost-estimate)
  (let* ((init-queue (make-psq point-order <))
         (init-queue (psq-set init-queue (cons start-point 0) (cost-estimate start-point end-point))))
    (let loop ((n 0) (queue init-queue))
      (receive (next next-queue) (psq-pop queue)
        (match next ((point . t)
         ; (display (list point t (psq-ref queue (cons point t)))) (newline)
          (if
            (or (> t 300) (equal? point end-point))
            t
            (loop (1+ n) (fold (lambda (next q) (psq-set q (cons next (1+ t)) (+ 1 t (cost-estimate next end-point)))) next-queue (next-states point (1+ t))))
            )
        ))))))

(define (next-points grid blizzards)
  (lambda (p t)
    (filter
      (lambda (p) (and (equal? (hash-ref grid p) #\.) (not (hash-ref blizzards (cons t p)))))
      (cons p (grid-neighbours p)))))

; This is very annoying!
(define (point-order p1 p2)
  (cond ((< (point-x (car p1)) (point-x (car p2))) #t)
        ((> (point-x (car p1)) (point-x (car p2))) #f)
        (else (cond ((< (point-y (car p1)) (point-y (car p2))) #t)
                    ((> (point-y (car p1)) (point-y (car p2))) #f)
                    (else (< (cdr p1) (cdr p2)))))))



(define (extract-blizzards grid)
  (filter identity (map point-to-blizzard (grid-items grid))))

(define (update-blizzards bounds blizzards)
  (map (lambda (blizz) (cons (wrap-around bounds (point+ (car blizz) (cdr blizz))) (cdr blizz))) blizzards))

(define (wrap-around bounds point)
  (cond ((= (point-x point) (grid-min-x bounds)) (make-point (1- (grid-max-x bounds)) (point-y point)))
        ((= (point-x point) (grid-max-x bounds)) (make-point (1+ (grid-min-x bounds)) (point-y point)))
        ((= (point-y point) (grid-min-y bounds)) (make-point (point-x point) (1- (grid-max-y bounds))))
        ((= (point-y point) (grid-max-y bounds)) (make-point (point-x point) (1+ (grid-min-y bounds))))
        (else point)))

(define (blizzard? c) (or (equal? c #\v) (equal? c #\<) (equal? c #\>) (equal? c #\^)))

(define (clear-blizzards! grid)
  (for-each (lambda (kv) (if (blizzard? (cdr kv)) (hash-set! grid (car kv) #\.))) (grid-items grid)))

(define (add-blizzards! grid blizzards)
  (for-each (lambda (kv) (hash-set! grid (car kv) (to-blizzard-symbol (cdr kv)))) blizzards))

(define (to-blizzard-symbol v)
  (match v (($ <point> 1 0) #\>)
           (($ <point> -1 0) #\<)
           (($ <point> 0 1) #\v)
           (($ <point> 0 -1) #\^)
         ))


(define point-to-blizzard
  (match-lambda ((p . #\.) #f)
                ((p . #\#) #f)
                ((p . #\^) (cons p (make-point 0 -1)))
                ((p . #\<) (cons p (make-point -1 0)))
                ((p . #\>) (cons p (make-point 1 0)))
                ((p . #\v) (cons p (make-point 0 1)))
                ))

(define (read-board) (read-grid (lambda (x) x)))

(define (start-point grid) (first-empty grid 1))
(define (end-point grid) (first-empty grid (maximum (map point-y (grid-keys grid)))))

(define (first-empty grid row) (caar (filter (lambda (p) (and (equal? (point-y (car p)) row) (equal? (cdr p) #\.))) (grid-items grid))))
