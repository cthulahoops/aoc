(add-to-load-path ".")
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (aoc))

(define (move-value move) (match move ('rock 1) ('paper 2) ('scissors 3)))
(define (winner p1 p2)
  (let ((v1 (move-value p1))
        (v2 (move-value p2)))
    (match (modulo (- v1 v2) 3) (0 'draw) (1 'p1) (2 'p2))))

(define (outcome-value outcome) (match outcome ('me 0) ('draw 1) ('elf 2)))
(define (move-needed elf-move outcome)
  (let ((v1 (move-value elf-move))
        (v2 (outcome-value outcome)))
    (match (modulo (- v1 v2) 3) (0 'rock) (1 'paper) (2 'scissors))))

(define (parse-elf-move move)
  (match move ("A" 'rock) ("B" 'paper) ("C" 'scissors)))
(define (parse-my-move move)
  (match move ("X" 'rock) ("Y" 'paper) ("Z" 'scissors)))
(define (parse-outcome outcome)
  (match outcome ("X" 'elf) ("Y" 'draw) ("Z" 'me)))

(define (winner-score p1 p2) (match (winner p1 p2) ('p1 6) ('draw 3) ('p2 0)))
(define (round-score elf me) (+ (winner-score me elf) (move-value me)))

(define (parse-line line) (let ((moves (string-split line #\space))) (list (parse-elf-move (car moves)) (parse-my-move (cadr moves)))))
(define input-lines (with-input-from-file "input/2" read-lines))
(define input1 (map parse-line input-lines))

(define part1 (sum (map (lambda (moves) (apply round-score moves)) input1)))
(format #t "Part 1: ~d\n" part1)

(define (parse-line-correctly line) (let ((moves (string-split line #\space))) (list (parse-elf-move (car moves)) (parse-outcome (cadr moves)))))
(define input2 (map parse-line-correctly input-lines))

(define line-score (match-lambda ((elf-move outcome) (round-score elf-move (move-needed elf-move outcome)))))

(define part2 (sum (map line-score input2)))
(format #t "Part 2: ~d\n" part2)
