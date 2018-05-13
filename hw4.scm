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

;2.20

;2.22

;2.23
