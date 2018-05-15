;2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

;2.8
; interval subtraction: [a, b] - [c, d] = [a - d, b - c]
(define (subtract-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y)))) 

;2.10

(define (spans-zero? i)
  (and (<= (upper-bound i) 0)
       (>= (lower-bound i) 0)))

(define (div-interval-improved x y)
  (if (spans-zero? y)
    (error "Error: the dividing interval cannot span 0")
    (div-interval x y)))
  
;2.12

(define (make-center-percent center tolerance)
 (let ((upper (+ center (* tolerance center)))
       (lower (- center (* tolerance center))))
   (make-interval lower upper)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((c (center i)))
    (/ (- (upper-bound i) c) c)))

;2.17

(define (last-pair l)
  (if (empty? (cdr l)) l
    (last-pair (cdr l))))

;2.20 running into an issue with dot-notation

(define (same-parity divisor . l)
  (print l)
  (define (same-parity-helper divisor result l)
    (cond ((empty? l) result)
          ((= (remainder (car l)  divisor) 0) (same-parity-helper divisor (append result (list (car l))) (cdr l)))
          (else (same-parity-helper divisor result (cdr l)))))
  (same-parity-helper divisor '() l))

;2.21

(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))
;2.22

(define (sqaure x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
    (iter (cdr things)
          (cons (square (car things))
            answer ))))
  (iter items '()))

;The reason this comes out backwards is because cons adds an element to the front of an existing list and since this iterative implementation doesn't "cons-up" recursively, the answer displays as "backwards"

;2.23

(define (for-each proc items)
  (if (null? items) #t
    (and (proc (car items))
      (for-each proc (cdr items)))))

;define substitute

; first pass
(define (substitute l old new)
  (define (sub s old new)
    (map (lambda (word) (if (equal? word old)  new word)) s))
  
  (map (lambda (item) (if (list? item) (sub item old new)
    (if (equal? item old) new item))) l))

;second pass
(define (substitute l old new)
  (cond ((null? l) '())
        ((list? (car l)) (cons (substitute (car l) old new)
                               (substitute (cdr l) old new)))
        ((equal? (car l) old) (cons new (substitute (cdr l) old new)))
        (else (cons (car l) (substitute (cdr l) old new)))))

; define substitute2
(define (substitute2 l old new)
  (define (get-index i items)
    (if (= i 1) (car items)
      (get-index (- i 1) (cdr items))))  

  (define (in-list? w i items)
    (if (null? items) 0
      (if (equal? (car items) w) i
        (in-list? w (+ i 1) (cdr items)))))

  (cond ((null? l) '())
        ((list? (car l)) (cons (substitute2 (car l) old new)
                               (substitute2 (cdr l) old new)))
        ((> (in-list? (car l) 1 old) 0)  (cons (get-index (in-list? (car l) 1 old) new) 
                                        (substitute2 (cdr l) old new)))
        (else (cons (car l) (substitute2 (cdr l) old new)))))
