(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 vlist))

(define-immutable-record-type <step>
  (make-step direction count)
  step?
  (direction step-direction)
  (count step-count))

(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))

(define-immutable-record-type <rope>
  (make-rope knots history)
  rope?
  (knots rope-knots)
  (history rope-history))

(define (read-steps)
  (pipe>
    (read-lines)
    (map parse-step)))

(define (parse-step line)
  (let ((parts (string-split line #\space)))
    (make-step (car parts) (string->number (cadr parts)))))

(define (apply-direction direction point)
  (match direction ("U" (set-point-y point (+ (point-y point) 1)))
                   ("D" (set-point-y point (- (point-y point) 1)))
                   ("L" (set-point-x point (- (point-x point) 1)))
                   ("R" (set-point-x point (+ (point-x point) 1)))))

(define (step-towards head tail)
  (let* ((dx (- (point-x head) (point-x tail)))
         (dy (- (point-y head) (point-y tail)))
         (furthest (max (abs dx) (abs dy)))
         (step (if (<= furthest 1) (make-point 0 0) (make-point (sign dx) (sign dy)))))
    (point+ step tail)))

(define (follow-head head knots)
  (if (null? knots)
      (list)
      (let ((next-knot (car knots))
            (rest (cdr knots)))
        (cons (step-towards head next-knot) (follow-head next-knot rest)))))

(define (apply-direction-rope direction rope)
  (let* ((head (apply-direction direction (car (rope-knots rope))))
         (tail (follow-head head (cdr (rope-knots rope)))))
    (make-rope (cons head tail) (cons (last tail) (rope-history rope)))))

(define (apply-step step rope) (apply-n-times (step-count step) (lambda (x) (apply-direction-rope (step-direction step) x)) rope))

(define (simulate rope-length)
  (let*
    ((steps (read-steps))
     (rope (make-rope (replicate rope-length (make-point 0 0)) (list)))
     (final-rope (fold apply-step rope steps)))
    (count-unique (cons (last (rope-knots final-rope)) (rope-history final-rope)))))

(define (part1) (simulate 2))
(define (part2) (simulate 10))
