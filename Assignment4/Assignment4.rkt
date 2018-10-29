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
(define (next-k-items s k)
  (if (> k 0)
      (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))
      `()))

; kth-item
(define (kth-item s k)
      (if (> k 0)
	  (kth-item (cdr (s)) (- k 1))
	  (car (s))))

; negate-2-and-5
(define negate-2-and-5 (letrec ([f (lambda(x) 
				     (cond [(= (remainder x 2) 0)
					    (cons (* x -1) (lambda () (f (+ x 1))))]
					   [(= (remainder x 5) 0) 
					    (cons (* x -1) (lambda () (f (+ x 1))))]
					   [else 
					    (cons x (lambda () (f (+ x 1))))]))])
			 (lambda () (f 1))))

; key-heart-star
(require 2htdp/planetcute)
(define key-heart-star (letrec ([f (lambda (img)
                                     (cond [(eq? img key)
                                            (cons key (lambda () (f heart)))]
                                           [(eq? img heart)
                                            (cons heart (lambda () (f yellow-star)))]
                                           [(eq? img yellow-star)
                                            (cons yellow-star (lambda () (f key)))]))])
                         (lambda () (f key))))
                                          
; two-pairs-stream
(define (two-pair-stream s) 
  (lambda () (cons (cons 2 (car (s))) (two-pair-stream (cdr (s))))))
  
	

