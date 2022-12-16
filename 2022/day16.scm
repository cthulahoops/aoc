(add-to-load-path ".")
(use-modules (aoc))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 regex))

(define-record-type <valve>
  (make-valve id flow-rate tunnels)
  valve?
  (id valve-id)
  (flow-rate valve-flow-rate)
  (tunnels valve-tunnels))

(define-immutable-record-type <state>
  (make-state time-left locations open flow-rate)
  state?
  (time-left state-time-left set-state-time-left)
  (locations state-locations set-state-locations)
  (open state-open)
  (flow-rate state-flow-rate))

(define (memo f)
  (let ((cache (make-hash-table)))
    (lambda (volcano x)
      (if (not (hash-ref cache x)) (hash-set! cache x (f volcano x)))
      (hash-ref cache x))))

(define (state-location state) (car (state-locations state)))

(define (read-input) (map parse-line (read-lines)))
(define (match-number match count) (string->number (match:substring match count)))
(define (parse-valve-list valve-list) (map (lambda (x) (string-trim-both x char-set:whitespace)) (string-split valve-list #\,)))

(define (parse-line line)
  (let ((match (string-match "Valve ([A-Z][A-Z]) has flow rate=([0-9]*);.*valves? (.*)" line)))
    (make-valve (match:substring match 1) (match-number match 2) (parse-valve-list (match:substring match 3)))))

(define (set-state-location state tunnel) (make-state (state-time-left state) (cons tunnel (cdr (state-locations state))) (state-open state) (state-flow-rate state)))

(define (location-tunnels volcano state)
  (valve-tunnels (hash-ref volcano (state-location state))))

(define (is-open? state)
  (member (state-location state) (state-open state)))

(define (openable? volcano state)
  (cond
    ((is-open? state) #f)
    ((= 0 (valve-flow-rate (hash-ref volcano (state-location state)))) #f)
    (else #t)))

(define (open-valve volcano state)
  (make-state
    (state-time-left state)
    (state-locations state)
    (sort (cons (state-location state) (state-open state)) string<)
    (+ (state-flow-rate state) (valve-flow-rate (hash-ref volcano (state-location state))))))

(define (next-states volcano state)
  (let ((moves (map (partial set-state-location state) (location-tunnels volcano state))))
    (if (openable? volcano state)
        (cons (open-valve volcano state) moves)
        moves)))

(define (state-helpers state) (set-state-locations state (cdr (state-locations state))))

(define (next-states* volcano state)
  (if
    (= (length (state-locations state)) 1)
    (next-states volcano state)
    (append-map
      (lambda (x) (map (lambda (s) (set-state-locations s (sort (cons (car (state-locations x)) (state-locations s)) string<))) (next-states volcano (state-helpers x))))
      (next-states volcano state))))

(define (step-time state) (set-state-time-left state (- (state-time-left state) 1)))

(define (released-pressure volcano state)
  (cond
    ((= 0 (state-time-left state)) 0)
   ; ((= 81 (state-flow-rate state)) (* (state-flow-rate state) (state-time-left state)))
    ((= 201 (state-flow-rate state)) (* (state-flow-rate state) (state-time-left state)))
    (else (+ (state-flow-rate state) (maximum (map (lambda (x) (released-pressure volcano x)) (map step-time (next-states* volcano state))))))))

(define released-pressure (memo released-pressure))

(define (part1)
  (let* ((input (read-input))
         (volcano (alist->hash-table (map (lambda (x) (cons (valve-id x) x)) input)))
         )
    (released-pressure volcano (make-state 30 (list "AA") (list) 0))))

(define (part2)
  (let* ((input (read-input))
         (volcano (alist->hash-table (map (lambda (x) (cons (valve-id x) x)) input)))
         (best (sum (map valve-flow-rate input)))
         )
    (display best)
    (newline)
    (released-pressure volcano (make-state 26 (list "AA" "AA") (list) 0))))
