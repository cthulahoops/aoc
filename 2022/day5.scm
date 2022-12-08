(add-to-load-path ".")
(use-modules (ice-9 regex))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-1))
(use-modules (aoc))

(define-record-type <instruction>
  (make-instruction count origin destination)
  instruction?
  (count instruction-count)
  (origin instruction-origin)
  (destination instruction-destination))

; Part one (crane 9000)
(define (apply-instructions-9000 instructions stacks) (fold apply-instruction-9000 stacks instructions))
(define (apply-instruction-9000 instruction stacks)
  (let loop ((count (instruction-count instruction)) (stacks stacks))
    (if (= 0 count)
        stacks
        (loop (- count 1) (move-items 1 (instruction-origin instruction) (instruction-destination instruction) stacks)))))

; Part two (crane 9001)
(define (apply-instructions-9001 instructions stacks) (fold apply-instruction-9001 stacks instructions))
(define (apply-instruction-9001 instruction stacks)
  (move-items (instruction-count instruction) (instruction-origin instruction) (instruction-destination instruction) stacks))

; Stack manipulation
(define (move-items count origin destination stacks)
  (let* (
        (pop-result (pop-n-from-stack count origin stacks))
        (value (car pop-result))
        (popped-stacks (cdr pop-result)))
    (push-n-to-stack destination value popped-stacks)))

(define (pop-n-from-stack count stack-number stacks)
  (let* (
      (stack (cdr (vhash-assoc stack-number stacks)))
      (value (list-head stack count))
      (rest (drop stack count))
      (new-stacks (vhash-cons stack-number rest (vhash-delete stack-number stacks))))
    (cons value new-stacks)))

(define (push-n-to-stack stack-number items stacks)
  (let* (
      (stack (cdr (vhash-assoc stack-number stacks)))
      (updated-stack (append items stack)))
    (vhash-cons stack-number updated-stack (vhash-delete stack-number stacks))))

; Parsing:
(define (to-stacks stack-picture)
  (pipe>
    stack-picture
    (zip-lists)
    (filter is-stack?)
    (map (lambda (x) (cons (character->number (stack-number x)) (filter char-alphabetic? x))))
    (alist->vhash)
    ))

(define (stack-number x) (car (last-pair x)))
(define (is-stack? x) (char-numeric? (stack-number x)))

(define (parse-instruction instruction)
  (let ((match (string-match "^move ([0-9]+) from ([0-9]) to ([0-9])" instruction)))
    (make-instruction
      (string->number (match:substring match 1))
      (string->number (match:substring match 2))
      (string->number (match:substring match 3)))))

(define (read-loading-instructions)
  (list
    (to-stacks (map string->list (read-block)))
    (map parse-instruction (read-lines))))

; Output
(define (stack-heads stacks)
    (list->string (map cadr (sort (vlist->list stacks) (lambda (x y) (< (car x) (car y)))))))

(define (part1) (part apply-instructions-9000))
(define (part2) (part apply-instructions-9001))

(define (part crane-function)
  (let* (
        (input (read-loading-instructions))
        (stacks (car input))
        (instructions (cadr input))
        (final-stacks (crane-function instructions stacks)))
    (stack-heads final-stacks)))
