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
  (make-rope head tail history)
  point?
  (head rope-head)
  (tail rope-tail)
  (history rope-history))

(define (point-+ p1 p2) (make-point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))

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


(define (single-step value)
  (cond ((< value -1) -1)
        ((> value 1) 1)
        (else value)))

(define (step-towards head tail)
  (let* ((dx (- (point-x head) (point-x tail)))
         (dy (- (point-y head) (point-y tail)))
         (furthest (max (abs dx) (abs dy)))
         (step (if (<= furthest 1) (make-point 0 0) (make-point (single-step dx) (single-step dy)))))
    (point-+ step tail)))

(define (apply-direction-rope direction rope)
  (let* ((head (apply-direction direction (rope-head rope)))
         (tail (step-towards head (rope-tail rope))))
    (display direction)
    (display head)
    (display tail)
    (newline)
    (make-rope head tail (cons (rope-tail rope) (rope-history rope)))))

(define (apply-n-times n f v)
  (let loop ((n n) (v v))
    (if (= n 0)
        v
        (loop (- n 1) (f v)))))
(define (apply-step step rope) (display step) (newline) (apply-n-times (step-count step) (lambda (x) (apply-direction-rope (step-direction step) x)) rope))

; Counter
(define (make-counter) (alist->vhash (list)))
(define (count-items key value counter)
  (let* ((old-count (counter-get key counter))
         (new-count (+ old-count value)))
         (vhash-cons key new-count (vhash-delete key counter))))
(define (counter-get key counter)
  (cdr (or (vhash-assoc key counter) (cons key 0))))
(define counter->list vlist->list)

(define (unique-points points)
  (pipe>
    (fold (lambda (point counter) (count-items point 1 counter)) (make-counter) points)
    (counter->list)
    (length)))

(define (part1)
  (let*
    ((steps (read-steps))
     (rope (make-rope (make-point 0 0) (make-point 0 0) (list)))
     (final-rope (fold apply-step rope steps)))
     (display-lines (rope-history final-rope))
     (display (rope-tail final-rope))
    (unique-points (rope-history final-rope))))

(define (part2) 0)
