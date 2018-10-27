;#lang racket

; TODO do comments because apparantly without them I'm just getting really lucky about the code I'm writing
; downseries
(define (downseries step high low)
  (if (< high low)
  	`()
	(cons high (downseries step (- high step) low))))

; meow-string-map
(define (meow-string-map lst)
  (map (lambda (i) 
	 (string-append i "meow")) lst))

