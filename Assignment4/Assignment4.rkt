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
  (if (> k -1) 
      (cons (stream-ref s k) (next-k-items s (- k 1)))
      `()))


	  
