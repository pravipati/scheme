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

  (define (best-total hand)
    ;💡 what if we just add 10 at the end if the hand is < or = 11?
    (define (card-to-num card)
      (cond ((equal? (string-ref card 0) #\a) 1)
            ((or (equal? (string-ref card 0) #\a)
                 (equal? (string-ref card 0) #\j)
                 (equal? (string-ref card 0) #\q)
                 (equal? (string-ref card 0) #\k))
              10)
            (else (string->number (substring card 0 (- (string-length card) 1))))))

    (define (total hand)
      (if (null? hand) 0
        (+ (card-to-num (string (car hand))) (total (cdr hand)))))

    (define (has-ace? hand)
      (cond ((null? hand) #f)
            ((equal? (string-ref (string (car hand)) 0) #\a) #t)
            (else (has-ace? (cdr hand)))))

    (if (and (or (equal? (total hand) 11)
                 (< (total hand) 21))
             (has-ace? hand))
        (+ 10 (total hand))
      (total hand))
  )

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
  (shuffle (make-ordered-deck) 52) )
