(add-to-load-path ".")
(use-modules (aoc))
(use-modules (grid))
(use-modules (pfds psqs))
(use-modules (srfi srfi-1))
(use-modules (ice-9 receive))
(use-modules (ice-9 match))

(define (part1)
  (a-star (make-point 0 0) (make-point 5 0) grid-neighbours manhatten-distance))
(define (part2) 0)

(define (a-star start-point end-point next-states cost-estimate)
  (let* ((init-queue (make-psq point-order <))
         (init-queue (psq-set init-queue (cons start-point 0) (cost-estimate start-point end-point))))
    (let loop ((n 0) (queue init-queue))
      (receive (next next-queue) (psq-pop queue)
        (match next ((point . t)
          (display point)
          (display next-queue)
          (newline)
          (if
            (equal? point end-point)
            t
            (loop (1+ n) (fold (lambda (next q) (psq-set q (cons next (1+ t)) (+ t (cost-estimate next end-point)))) next-queue (list (car (next-states point)))))
            )
        ))))))

(define (point-order p1 p2) (< (point-x (car p1)) (point-x (car p2))))
