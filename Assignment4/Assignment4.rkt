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

