(add-to-load-path ".")
(use-modules (aoc))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))

(define-record-type <blueprint>
  (make-blueprint id costs)
  blueprint?
  (id blueprint-id)
  (costs blueprint-costs)
  )

(define-record-type <amount>
  (make-amount ore clay obsidian geode)
  amount?
  (ore amount-ore)
  (clay amount-clay)
  (obsidian amount-obsidian)
  (geode amount-geode)
  )

(define (amount+ a1 a2) (make-amount (+ (amount-ore a1) (amount-ore a2))
                                     (+ (amount-clay a1) (amount-clay a2))
                                     (+ (amount-obsidian a1) (amount-obsidian a2))
                                     (+ (amount-geode a1) (amount-geode a2))))

(define (amount- a1 a2) (make-amount (- (amount-ore a1) (amount-ore a2))
                                     (- (amount-clay a1) (amount-clay a2))
                                     (- (amount-obsidian a1) (amount-obsidian a2))
                                     (- (amount-geode a1) (amount-geode a2))))

(define (amount-negative? a) (or (< (amount-ore a) 0) (< (amount-clay a) 0) (< (amount-obsidian a) 0) (< (amount-geode a) 0)))

(define-immutable-record-type <state>
  (make-state time materials robots)
  state?
  (time state-time)
  (materials state-materials set-state-materials)
  (robots state-robots set-state-robots))

(define (match-number match count) (string->number (match:substring match count)))

(define
  line-regex
  "Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.")

(define (parse-blueprint line)
  (let* ((match (string-match line-regex line))
         (match-number (partial match-number match)))
    (make-blueprint
      (match-number 1)
      (list (list (make-amount 1 0 0 0) (make-amount (match-number 2) 0 0 0))
            (list (make-amount 0 1 0 0) (make-amount (match-number 3) 0 0 0))
            (list (make-amount 0 0 1 0) (make-amount (match-number 4) (match-number 5) 0 0))
            (list (make-amount 0 0 0 1) (make-amount (match-number 6) 0 (match-number 7) 0))))))


(define (run-action state max-ore-robots build cost)
  (if (and (equal? build (make-amount 1 0 0 0)) (>= (amount-ore (state-robots state)) max-ore-robots))
      #f
    (let ((new-materials (amount- (state-materials state) cost)))
      (cond ((<= (state-time state) 0) state)
            ((amount-negative? new-materials) (run-action (make-state (- (state-time state) 1) (amount+ (state-materials state) (state-robots state)) (state-robots state)) max-ore-robots build cost))
            (else (make-state (- (state-time state) 1) (amount+ new-materials (state-robots state)) (amount+ (state-robots state) build)))))))

(define (max-ore-robots actions) (maximum (cdr (map (compose amount-ore cadr) actions))))

(define (next-states blueprint-actions state)
  (filter identity (map (lambda (purchase-cost) (apply (partial run-action state (max-ore-robots blueprint-actions)) purchase-cost)) blueprint-actions)))

(define (memo f)
  (let ((cache (make-hash-table)))
    (lambda args
      (if (not (hash-ref cache args)) (hash-set! cache args (apply f args)))
      (hash-ref cache args))))

(define (most-geodes blueprint-actions state)
  (if (<= (state-time state) 0)
      state
      (maximum-by (compose amount-geode state-materials) (map (partial most-geodes blueprint-actions) (next-states blueprint-actions state))))
      )

(define most-geodes (memo most-geodes))

(define (part1)
  (let* ((blueprints (map parse-blueprint (read-lines)))
         (init-state (make-state 24 (make-amount 0 0 0 0) (make-amount 1 0 0 0)))
         (geode-results (map (lambda (b) (display (blueprint-id b)) (newline) (cons (blueprint-id b) (most-geodes (blueprint-costs b) init-state))) blueprints))
        )
    (sum (map (lambda (pair) (* (car pair) (amount-geode (state-materials (cdr pair))))) geode-results))
    ))

(define (part2)
  (let* ((blueprints (take (map parse-blueprint (read-lines)) 3))
         (init-state (make-state 32 (make-amount 0 0 0 0) (make-amount 1 0 0 0)))
         (geode-results (map (lambda (b) (display (blueprint-id b)) (newline) (cons (blueprint-id b) (most-geodes (blueprint-costs b) init-state))) blueprints))
        )
    (apply * (map (compose amount-geode state-materials cdr) geode-results))
    ))
