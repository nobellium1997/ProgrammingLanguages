;#lang racket

; downseries that takes three arguments, step, high, low all assumed to be numbers. Further, step is positive. downseries produces a list of numbers from high to low (including high and possibly low) separated by step and in sorted descending order.  
(define (downseries step high low)
  (if (< high low)
  	`()
	(append (list high) (downseries step (- high step) low))))

