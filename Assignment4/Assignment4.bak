#lang racket

; downseries
; this function takes three arguements, a high step and low which are all numbers
; step has to be positive
; this function returns a list of numbers from high to low separated by step
; all I did was return an empty list if high was less than low, otherwise 
; cons high and recursively call downseries with high minus step
(define (downseries step high low)
  (if (< high low)
  	`()
	(cons high (downseries step (- high step) low))))

; meow-string-map
; this function just takes a list of strings and appends meow to the end of them
; I just used the map function with an anonymous function that takes an element in the list
; and appends meow... 
(define (meow-string-map lst)
  (map (lambda (elem) 
	 (string-append elem "meow")) lst))

; list-ref-div
; this function takes a list and a number. It terminates if the number is negative or if
; the list is empty
; otherwise it returns the ith element of the list and i is the quotient 
; produced when dividing by n and the list's length
; all I did here was use list-ref with the quotient and length function for i
(define (list-ref-div lst n)
  (if (< n 0)
      (error "list-ref-div: negative number")
      (if (= (length lst) 0)
	  (error "list-ref-div:empty list")
	  (list-ref lst (quotient n (length lst))))))

; next-k-items
; takes two arguements, a stream and a number and it produces a list
; with the next k elements from the stream
; all I did was call car on the stream, added it to the list with cons
; along with a recursive call to the function part of the stream
; and subtracted k so that it would only do k elements
(define (next-k-items s k)
  (if (> k 0)
      (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))
      `()))

; kth-item
; takes a stream and a k and returns the kth item of the stream
; similar to the last one except I only return when k is 0 
(define (kth-item s k)
      (if (> k 0)
	  (kth-item (cdr (s)) (- k 1))
	  (car (s))))

; negate-2-and-5
; stream is fairly straight forward, I return an initial pair of 1 and a function
; and the function checks if it is divisible by 2 or 5. If it is I just cons the negative
; value to the stream 
(define negate-2-and-5 (letrec ([f (lambda(x) 
				     (cond [(= (remainder x 2) 0)
					    (cons (* x -1) (lambda () (f (+ x 1))))]
					   [(= (remainder x 5) 0) 
					    (cons (* x -1) (lambda () (f (+ x 1))))]
					   [else 
					    (cons x (lambda () (f (+ x 1))))]))])
			 (lambda () (f 1))))

; key-heart-star
; this one returns an image and a function which checks the image and passes in the next image
; in the stream recursively. It should return images in the order of key-heart-star and loop back again 
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
; this takes a stream and returns a new stream where each element in the stream is the pair
; 2 and the element returned from the stream
; I simply had it return a function where it returns the pair of 2 and the stream val and then 
; the function with the rest of the stream 
(define (two-pair-stream s) 
  (lambda () (cons (cons 2 (car (s))) (two-pair-stream (cdr (s))))))
  
; spin-stream
; the function underneath was just to help concatinate the two streams together
; while handling overflow by taking the modulo of n and the length
(define (spin-stream-helper xs ys n)
  (lambda () (cons (cons (list-ref xs (modulo n (length xs))) (list-ref ys (modulo n (length ys)))) (spin-stream-helper xs ys (+ n 1)))))

; the real function takes in two streams and returns a pair of elements from each list
; which rotates forever
; I simply used a helper function that would concatinate the lists and had a value n to get the certain element
; at the certain position
(define (spin-stream xs ys)
  (spin-stream-helper xs ys 0))

; kvpv-lookup
; takes two arguments a value and a vector. It's basically an assoc function
; for vectors with key-value pairs. Again I use a helper here with a position value
; so that I can cycle through the vector
; if n isn't equal check the car of the vector at pos n and see if it's equal to v
; else keep looking with n+1
(define (kvpv-lookup-helper v vec n)
  (cond [(= n (vector-length vec)) 
	 #f]
	[(pair? (vector-ref vec n)) 
	 (if (equal? v (car (vector-ref vec n)))
	     (vector-ref vec n)
	     (kvpv-lookup-helper v vec (+ n 1)))]
	[else 
	  (kvpv-lookup-helper v vec (+ n 1))]))
		
(define (kvpv-lookup v vec)
  (kvpv-lookup-helper v vec 0))

; cached-lookup
; this takes a list and a number n to create the vector
; this returns a function that takes in a value and list of pairs similar to assoc
; this will first check to see if it's in the cache and return #t with the pair value
; if it doesn't find it in the cache it will look at the list and if it does find it there
; it will set it in the vector set the position and return #f with the pair
; else just false 
; I just define a cache and cache-pos for the memozation 
; and search using kvpv-lookup and assoc for the two let values
; if the let value is a pair I return it 
; else I check the list-find value and if it's a pair it goes through the begin () part 
; else return false
(define (cached-lookup lst n)
  (let ([cache (make-vector n #f)]
	[cache-pos 0])
    (lambda (v lst2)
      (let ([cache-find (kvpv-lookup v cache)]
	    [list-find (assoc v lst2)])
      (if (pair? cache-find)
	  (cons #t cache-find)
	  (if (pair? list-find)
	      (begin 
		      (vector-set! cache (modulo cache-pos (vector-length cache)) list-find)
		      (set! cache-pos (+ cache-pos 1))
		      (cons #f list-find))
	      #f))))))
      
