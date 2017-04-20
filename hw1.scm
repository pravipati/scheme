;Write  a  procedure  squares that takes  a  sentence  of numbers  as  its  argument  and returns a sentence of the squares  of the numbers:

(define (squares nums) 
  (cond ((null? nums) '())
        (else (cons (square (car nums)) 
                    (squares (cdr nums))))))


(define (square x) 
  (* x x))

;Write a procedure  switch that takes a sentence as its argument and returns a sentence in which every instance  of the words I or me is replaced  by you, while every instance  of you is replaced  by me except  at the beginning  of the sentence,  where it’s  replaced  by I. (Don’t worry about capitalization of letters.)

(define (switch-helper words first?)
  (cond ((null? words) '())
        ((and 
          first?
          (or (equal? (car words) 'You) (equal? (car words) 'you))) (cons 'I (switch-helper (cdr words) #f)))
        ((equal? (car words) 'you) (cons 'me (switch-helper (cdr words) #f)))
        ((or (equal? (car words) 'I) (equal? (car words) 'me)) (cons 'you (switch-helper (cdr words) #f)))
        (else (cons (car words) (switch-helper (cdr words) #f)))))

(define (switch words)
  (switch-helper words #t))

;Write  a predicate  ordered? that takes  a sentence  of numbers  as its  argument  and returns a true value if the numbers  are in ascending  order,  or a false value otherwise.

(define (ordered? nums)
  (cond ((or (equal? nums '())
             (equal? (cdr nums) '())) #t)
        ((> (car nums)
           (car (cdr nums))) #f)
        (else (ordered? (cdr nums)))))

;Write a procedure  ends-e that takes a sentence as its argument and returns a sentence containing only those words of the argument whose last letter is E

(define (ends-e words)
  (cond ((equal? words '()) '())
        ((ends-e-helper (string (car words))) (cons (car words) (ends-e (cdr words))))
        (else (append '() (ends-e (cdr words))))))

(define (ends-e-helper word)
  (cond ((= (string-length word) 0) #f)
    (else (equal? '#\e (string-ref word (- (string-length word) 1))))))

;Your mission is to devise a test that will tell you whether Scheme’s and and or are special forms or ordinary  functions.  This is a somewhat tricky problem,  but it’ll get you thinking about the evaluation process more deeply than you otherwise might.

(or 1 skjsd) ;=> 1

(and #f sndfi sdfsfd) ;=> #f

;The above examples prove that `or` and `and` are both special forms. Both expressions contain invalid variables that in an ordinary function would cause an error. 

