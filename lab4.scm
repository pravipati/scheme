; 2.2

(define (midpoint-segment s)
  (cons (/ (+ (x-point (start-segment s))
              (x-point (end-segment s))) 2)
        (/ (+ (y-point (start-segment s))
              (y-point (end-segment s))) 2))) 

(define (make-segment x y) 
  (list x y))

(define (start-segment s) 
  (car s))

(define (end-segment s) 
  (car (cdr s)))

(define (make-point x y) 
  (list x y))

(define (x-point p) 
  (car p))

(define (y-point p) 
  (car (cdr p)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; 2.3

(define (make-rect a b c d)
  (list (make-segment a b)
        (make-segment b c)
        (make-segment c d)
        (make-segment d a)))

(define (perimeter rect)
  (let ((a (get 0 rect))
        (b (get 1 rect))
        (c (get 2 rect))
        (d (get 3 rect)))

  (+ (distance (start-segment a) (end-segment a))
     (distance (start-segment b) (end-segment b))
     (distance (start-segment c) (end-segment c))
     (distance (start-segment d) (end-segment d)))))

(define (get index rect)
  (if (equal? index 0) (car rect)
    (get (- index 1) (cdr rect))))

(define (distance start end)
  (sqrt (+ (square (- (x-point end) (x-point start)))
           (square (- (y-point end) (y-point start))))))

; 2.4

(define (cdr z)
  (z (lambda (p q) q)))

; 2.18

(define (reverse l)
  (if (empty? l) '()
    (append (reverse (cdr l)) (list (car l)))))
