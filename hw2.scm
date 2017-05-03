; exercise 1.31(a)
; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to ππ using the formula

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
      (product term (next a) next b))))

; exercise 1.32(a)
;Show that sum and product (Exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
      (accumulate combiner null-value (next term) a next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

; exercise 1.33

(define (filtered-accumulate filter combiner null-value t erm a next b)
  (define (new-term arg) 
    (if (filter arg)
      (term arg)
    null-value))
  )

; exercise 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c )))

; exercise 1.41

(define (double proc)
  (lambda (x) (proc (proc x))))

; exercixe 1.42 

(define (compose proc1 proc2)
  (lambda (x) (proc1 (proc2 x))))

; exercise 1.43

(define (repeated proc n)
  (cond ((equal? n 1) (lambda (x) (proc x)))
        (else (compose proc (repeated proc (- n 1))))))

; *Note* you have to use compose to return a function definition back instead of evaluating `proc`

; exercise 1.46

;2.  a  higher-order   procedure  called  every that applies  an  arbitrary procedure,  given as an argument, to each word of an argument sentence.
(define (every proc args)
  (cond ((null? args) '())
    (else (cons (proc (car args))
          (every proc (cdr args))))))

; extra for experts

;(lambda (x) ((* x x))

