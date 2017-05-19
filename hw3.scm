; 1. squares iterative

(define (squares nums)

  (define (square num)
    (* num num))

  (define (squares-iter nums squared-nums)
    (if (null? nums) squared-nums
      (squares-iter (bf nums) (se squared-nums (square (first nums))))))

  (squares-iter nums '()))

; 2. multiply iterative

(define (multiply nums) 
  (define (multiply-iter nums total)
    (if (null? nums) total
      (multiply-iter (bf nums) (* total (first nums)))))
  (multiply-iter nums 1))

; 3. perfect number

(define (next-perf n)
  (define (sum-of-factors num factor total) 
      (cond ((> factor (- num 1)) total)
            ((zero? (modulo num factor)) (sum-of-factors num (+ 1 factor) (+ total factor)))
            (else (sum-of-factors num (+ 1 factor) total))))

  (if (equal? (sum-of-factors n 1 0) n) n
    (next-perf (+ 1 n))))

; 4. Abelson & Sussman exercise 1.16

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((zero? n) a) 
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))
  (fast-expt-iter b n 1))

; 5. product=b^(n−counter)

; 6. The result would be different when `amount` and `kinds-of-coins` are both 0
; The reason is because switching the conditions would result in a 1 being returned for
; those argument values whereas originally it would be 0