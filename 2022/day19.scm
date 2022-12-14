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

(define (map-amount f . amounts)
  (make-amount
    (apply f (map amount-ore amounts))
    (apply f (map amount-clay amounts))
    (apply f (map amount-obsidian amounts))
    (apply f (map amount-geode amounts))))

(define (amount+ a1 a2) (map-amount + a1 a2))
(define (amount- a1 a2) (map-amount - a1 a2))

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


(define (discard materials) (make-amount (min (amount-ore materials) 8) (min (amount-clay materials) 25) (min (amount-obsidian materials) 25) (amount-geode materials)))
(define (update-state state new-materials new-robots)
  (make-state (- (state-time state) 1) (discard new-materials) new-robots))

(define (run-action state max-ore-robots build cost)
  (if (or (and (equal? build (make-amount 1 0 0 0)) (>= (amount-ore (state-robots state)) max-ore-robots))
          (and (equal? build (make-amount 0 1 0 0)) (>= (amount-clay (state-robots state)) 10)))
      #f
    (let ((new-materials (amount- (state-materials state) cost)))
      (cond ((<= (state-time state) 0) state)
            ((amount-negative? new-materials) (run-action (update-state state (amount+ (state-materials state) (state-robots state)) (state-robots state)) max-ore-robots build cost))
            (else (update-state state (amount+ new-materials (state-robots state)) (amount+ (state-robots state) build)))))))

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
    (display-lines geode-results)
    (apply * (map (compose amount-geode state-materials cdr) geode-results))
    ))
