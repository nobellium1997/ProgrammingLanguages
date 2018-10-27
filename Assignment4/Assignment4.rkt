;#lang racket

; TODO do comments because apparantly without them I'm just getting really lucky about the code I'm writing
; downseries
(define (downseries step high low)
  (if (< high low)
  	`()
	(cons high (downseries step (- high step) low))))

; meow-string-map
(define (meow-string-map lst)
  (map (lambda (elem) 
	 (string-append elem "meow")) lst))

; list-ref-div
(define (list-ref-div lst n)
  (if (< n 0)
      (error "list-ref-div: negative number")
      (if (= (length lst) 0)
	  (error "list-ref-div:empty list")
	  (list-ref lst (quotient n (length lst))))))

; next-k-items
(define (next-k-items-helper s k)
  (if (< k (+ (stream-length s) 1))
   (if (> k 0) 
       (cons (stream-ref s (- k 1)) (next-k-items-helper s (- k 1)))
       `())
  `()))
(define (next-k-items s k)
  (reverse (next-k-items-helper s k)))

; kth-item
(define (kth-item s k)
  (if (< k (+ (stream-length s) 1))
      (if (> k 0)
	  (stream-ref s (- k 1))
	  0)
      0))

; negate-2-and-5
(define negate-2-and-5 (letrec ([f (lambda (x)
				     (cond [(< x 0) 
					    (cond [(= (remainder (- x 1) 2) 0) 
						   (cons x (lambda () (f (- x 1))))]
						  [(= (remainder (- x 1) 5) 0)
						   (cons x (lambda () (f (- x 1))))]
						  [else 
						   (cons x (lambda () (f (* (- x 1) -1))))])]
					   [else 
					     (cond [(= (remainder (+ x 1) 2) 0)
						    (cons x (lambda () (f (* (+ x 1) -1))))]
						   [(= (remainder (+ x 1) 5) 0)
						    (cons x (lambda () (f (* (+ x 1) -1))))]
						   [else 
						    (cons x (lambda () (f (+ x 1))))])]))])
			 (lambda () (f 1))))

