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

    (define (ace-shuffle hand)
      (cond ((null? hand) '())
            ((equal? (string-ref (string (car hand)) 0) #\a) 
              (append (ace-shuffle (cdr hand)) (cons (car hand) '())))
            (else (append (cons (car hand) '()) (ace-shuffle (cdr hand))))))
    
    ;pass ace-shuffled hand to helper
    (define (best-total-helper shuffled-hand running-total)

      (cond ((null? shuffled-hand) running-total)
            ((and (equal? (length shuffled-hand) 1)
                        (equal? (card-to-num (string (car shuffled-hand))) 1)
                        (< 21 (+ 11 running-total))) 
                   (+ 1 running-total))
            ((and (equal? (length shuffled-hand) 1)
                        (equal? (card-to-num (string (car shuffled-hand))) 1)
                        (> 21 (+ 11 running-total))) 
                   (+ 11 running-total))
            (else (best-total-helper (cdr shuffled-hand) (+ running-total (card-to-num (string (car shuffled-hand))))))))

    (best-total-helper (ace-shuffle hand) 0)
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
