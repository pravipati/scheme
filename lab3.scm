;1. define `type-check` to check inputs to procedures

(define (type-check proc pred arg)
  (and (pred arg) (proc arg)))

;2. define make-safe

(define (make-safe proc pred)
  (lambda (arg) (type-check proc pred arg)))

(define (count-change amount)
  (cc amount '(50 25 10 5 1)))


;3. Trace through cc

;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount
;                    (- kinds-of-coins 1))
;                  (cc (- amount
;                          (first-denomination kinds-of-coins))
;                      kinds-of-coins)))))

;4. use a sentence of coins instead

;(define (count-change amount)
;  (cc amount '(50 25 10 5 1)))

;(define (cc amount coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (empty? coins)) 0)
;        (else (+ (cc amount
;                    (bf coins))
;                  (cc (- amount
;                          (first coins))
;                      coins)))))

;5. start with pennies

(define (count-change amount)
  (cc amount 1))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (> kinds-of-coins 5)) 0)
        (else (+ (cc amount
                    (+ kinds-of-coins 1))
                  (cc (- amount
                          (first-denomination kinds-of-coins))
                      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))