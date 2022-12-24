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
    (next (next-points grid)))

  (display (start-point grid))
  (newline)
  (display (end-point grid))
  (newline)
  (display-grid identity grid)
  (a-star 0 (start-point grid) (end-point grid) next manhatten-distance)
 ))

(define (part2)
  (let* (
    (grid (read-board))
    (next (next-points grid))
    )

  (let*
    ((t1 (a-star 0 (start-point grid) (end-point grid) next manhatten-distance))
     (t2 (a-star t1 (end-point grid) (start-point grid) next manhatten-distance))
     (t3 (a-star t2 (start-point grid) (end-point grid) next manhatten-distance))
     ) t3)
 ))

(define (create-blizzard-history bounds count blizzards)
  (let ((blizzard-history (make-hash-table)))
    (do
      ((t 0 (1+ t)) (blizzards blizzards (update-blizzards bounds blizzards)))
      ((> t count))
      ; (display (list t blizzards))
      ; (newline)
      ; (clear-blizzards! grid)
      ; (add-blizzards! grid blizzards)
      ; (display-grid identity grid)
      (for-each (lambda (b) (hash-set! blizzard-history (cons t (car b)) #t)) blizzards)
      ; (newline)
    )
    blizzard-history))

(define (a-star start-time start-point end-point next-states cost-estimate)
  (let* ((init-queue (make-psq point-order <))
         (init-queue (psq-set init-queue (cons start-point start-time) (cost-estimate start-point end-point))))
    (let loop ((n 0) (queue init-queue))
      (receive (next next-queue) (psq-pop queue)
        (match next ((point . t)
         ; (display (list point t (psq-ref queue (cons point t)))) (newline)
          (if
            (equal? point end-point)
            t
            (loop (1+ n) (fold (lambda (next q) (psq-set q (cons next (1+ t)) (+ 1 t (cost-estimate next end-point)))) next-queue (next-states point (1+ t))))
            )
        ))))))

(define (next-points grid)
  (let* (
    (bounds (grid-bounds grid))
    (blizzards (extract-blizzards grid))
    (cycle-length (lcm (- (grid-max-x bounds) 2) (- (grid-max-y bounds) 2)))
    (blizzards (create-blizzard-history bounds cycle-length blizzards)))

    (lambda (p t)
      (filter
        (lambda (p) (and (hash-ref grid p) (not (equal? (hash-ref grid p) #\#)) (not (hash-ref blizzards (cons (modulo t cycle-length) p)))))
        (cons p (grid-neighbours p)))))
    )

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
