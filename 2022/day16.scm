(add-to-load-path ".")
(use-modules (aoc))
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
  (make-state time-left location open flow-rate)
  state?
  (time-left state-time-left)
  (location state-location set-state-location!)
  (open state-open set-state-open!)
  (flow-rate state-flow-rate))

(define (read-input) (map parse-line (read-lines)))
(define (match-number match count) (string->number (match:substring match count)))
(define (parse-valve-list valve-list) (map (lambda (x) (string-trim-both x char-set:whitespace)) (string-split valve-list #\,)))

(define (parse-line line)
  (let ((match (string-match "Valve ([A-Z][A-Z]) has flow rate=([0-9]*);.*valves? (.*)" line)))
    (make-valve (match:substring match 1) (match-number match 2) (parse-valve-list (match:substring match 3)))))

(define (follow-tunnel state tunnel) (make-state (- (state-time-left state) 1) tunnel (state-open state) (state-flow-rate state)))
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
    (- (state-time-left state) 1)
    (state-location state)
    (sort (cons (state-location state) (state-open state)) string<)
    (+ (state-flow-rate state) (valve-flow-rate (hash-ref volcano (state-location state))))))

(define (memo f)
  (let ((cache (make-hash-table)))
    (lambda (volcano x)
      (if (not (hash-ref cache x)) (hash-set! cache x (f volcano x)))
      (hash-ref cache x))))

(define (released-pressure volcano state)
  ; Open valve in current room.
  ; Follow any of the tunnels.
  (cond
    ((= 0 (state-time-left state)) 0)
    ; ((= 81 (state-flow-rate state)) (* (state-flow-rate state) (state-time-left state)))
    (else (let* ((next-states (map (partial follow-tunnel state) (location-tunnels volcano state)))
             (next-states (if (openable? volcano state) (cons (open-valve volcano state) next-states) next-states)))
        (+ (state-flow-rate state) (maximum (map (lambda (x) (released-pressure volcano x)) next-states)))))
      ))

(define released-pressure (memo released-pressure))

(define (part1)
  (let* ((input (read-input))
         (volcano (alist->hash-table (map (lambda (x) (cons (valve-id x) x)) input)))
         (best (sum (map valve-flow-rate input)))
         )
    (display best)
    (newline)
    (released-pressure volcano (make-state 30 "AA" '() 0))))

(define (part2) 0)
