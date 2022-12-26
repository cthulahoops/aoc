(define-module (grid)
               #:export (read-grid display-grid display-grid* grid-neighbours manhatten-distance grid-items grid-keys
                         grid-bounds grid-min-x grid-max-x grid-min-y grid-max-y))

(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (aoc))

(define (grid-keys hash) (hash-map->list (lambda (k v) k) hash))
(define (grid-items hash) (hash-map->list cons hash))

(define (parse-grid f board)
  (let* ((grid (make-hash-table))
         (parse-grid-cell (match-lambda* ((y (x . c)) (let ((grid-value (f c))) (if grid-value (hash-set! grid (make-point x y) grid-value))))))
         (parse-grid-line (match-lambda ((y . line) (map (partial parse-grid-cell y) (enumerate (string->list line)))))))
    (map parse-grid-line (enumerate board))
    grid))

(define (read-grid f) (parse-grid f (read-block)))

(define (display-grid* f hash-table)
  (let* (
         (points (grid-keys hash-table))
         (x0 (minimum (map point-x points)))
         (x1 (maximum (map point-x points)))
         (y0 (minimum (map point-y points)))
         (y1 (maximum (map point-y points)))
         (display-line 
           (lambda (y x0 x1)
              (display (list->string (map (lambda (x) (f (make-point x y) (hash-ref hash-table (make-point x y)))) (range x0 (1+ x1)))))
              (newline)))
        )
    (for-each (lambda (y) (display-line y (min 1 x0) x1)) (range (min 1 y0) (1+ y1)))))

(define (display-grid f hash-table)
  (let* (
         (points (grid-keys hash-table))
         (x0 (minimum (map point-x points)))
         (x1 (maximum (map point-x points)))
         (y0 (minimum (map point-y points)))
         (y1 (maximum (map point-y points)))
         (display-line 
           (lambda (y x0 x1)
              (display (list->string (map (lambda (x) (f (hash-ref hash-table (make-point x y)))) (range x0 (1+ x1)))))
              (newline)))
        )
    (for-each (lambda (y) (display-line y (min 1 x0) x1)) (range (min 1 y0) (1+ y1)))))

(define (grid-neighbours p) (map (partial point+ p) (list (make-point 1 0) (make-point 0 1) (make-point -1 0) (make-point 0 -1))))
(define (manhatten-distance p1 p2) (let ((p-diff (point- p1 p2))) (+ (abs (point-x p-diff)) (abs (point-y p-diff)))))

(define-record-type <grid-bounds>
  (make-grid-bounds min-x max-x min-y max-y)
  grid-bounds?
  (min-x grid-min-x)
  (max-x grid-max-x)
  (min-y grid-min-y)
  (max-y grid-max-y))

(define (grid-bounds grid)
  (let* (
     (points (grid-keys grid))
     (x0 (minimum (map point-x points)))
     (x1 (maximum (map point-x points)))
     (y0 (minimum (map point-y points)))
     (y1 (maximum (map point-y points)))
  ) (make-grid-bounds x0 x1 y0 y1)))


