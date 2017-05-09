(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (se (make-ordered-deck) "joker" "joker") 52) )

(define (best-total hand)

    (define (total hand)
      (if (null? hand) 0
        (+ (card-to-num (string (car hand))) (total (cdr hand)))))

    (define (has-ace? hand)
      (cond ((null? hand) #f)
            ((equal? (string-ref (string (car hand)) 0) #\a) #t)
            (else (has-ace? (cdr hand)))))

    (define (count-jokers hand)
      (cond ((null? hand) 0)
            ((equal? (first hand) "joker") (+ 1 (count-jokers (bf hand))))
            (else (+ 0 (count-jokers (bf hand))))))

    (cond ((< 21 (total hand)) (total hand))
          ((equal? (count-jokers hand) 2) 21)
          ((and (equal? (count-jokers hand) 1)
                (or (< 10 (total hand)) (has-ace? hand))) 21)
          ((equal? (count-jokers hand) 1) (+ 10 (total hand)))
          ((and (has-ace? hand)
                (> 12 (total hand))) (+ 10 (total hand)))
          (else (total hand)))
  )

(define (card-to-num card)
  (cond ((or (equal? card "joker")
             (equal? (string-ref card 0) #\a)) 1)
        ((or (equal? (string-ref card 0) #\j)
             (equal? (string-ref card 0) #\q)
             (equal? (string-ref card 0) #\k))
         10)
  (else (string->number (substring card 0 (- (string-length card) 1))))))

(define (stop-at-17 hand dealer-up-card)
  ((stop-at 17) hand dealer-up-card))

(define (stop-at n)
  (lambda (hand dealer-up-card) (< (best-total hand) n)))

(define (play-n strategy n)
  (define (play-n-iter strategy n accum)
    (if (= n 0) accum
      (play-n-iter strategy (- n 1) (+ accum (twenty-one strategy)))))

  (play-n-iter strategy n 0))

(define (dealer-sensitive hand dealer-up-card)
  (cond ((and (stop-at-17 hand dealer-up-card)
              (or (> (best-total (cons dealer-up-card '())) 6)
                  (= (best-total (cons dealer-up-card '())) 1))) #t)
        ((and ((stop-at 12) hand dealer-up-card) 
              (> (best-total (cons dealer-up-card '())) 1)) #t)
        (else #f)))

(define (valentine hand dealer-up-card)
  (if (has-suit? hand 'h) ((stop-at 19) hand dealer-up-card)
    ((stop-at 17) hand dealer-up-card)))

(define (has-suit? hand suit)
  (if (null? hand) #f
    (or (equal? (last (first hand)) suit)
        (has-suit? (bf hand) suit))))

(define (suit-strategy suit primary secondary)
  (lambda (hand dealer-up-card) 
    (if (has-suit? hand suit) (primary hand dealer-up-card)
      (secondary hand dealer-up-card))))

(define (valentine2 hand dealer-up-card)
  (suit-strategy 'h (stop-at 19) (stop-at 17)))

(define (majority s1 s2 s3)
  (lambda (hand dealer-up-card) 
    (or (and (s1 hand dealer-up-card) (s2 hand dealer-up-card))
        (s3 hand dealer-up-card))))

(define (reckless strategy)
  (lambda (hand dealer-up-card) (strategy (bl hand) dealer-up-card)))