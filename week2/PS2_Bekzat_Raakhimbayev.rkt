#lang slideshow

#| task 1|#

#| a|#
(define (converter n lst)
  (cond
    [(empty? lst) 0]
    [else (+ (* n (first lst)) (converter (* n 2) (rest lst)))]
    ))

(define (binary-to-decimal lst)
  (converter 1 (reverse lst))
  )

(binary-to-decimal '(1 0 1 1 0)) 

#| b |#
(define (xor x)
  (cond
    [(= 0 x) 1]
    [else 0]
    )
  )

(define (remove-leading-zero lst)
  (cond
    [(= 1 (first lst)) lst]
    [else (remove-leading-zero (rest lst))]
    )
  )

(define (countting lst)
  (cond
    [(empty? lst) 0]
    [else (+ (xor (first lst)) (countting (rest lst)) )]
    )
  )

(define (count-zeros lst)
  (countting (remove-leading-zero lst))
  )

(count-zeros '(0 0 0 1 0 1 1 0))

#| c |#



#| in this function val is value of first element in lst; count is number of consecutive substring with value 'val' ; lst is the our list |#
(define (encoding val count lst)
  (cond
    [(= 1 (length lst)) (list count)]
    [(= val (first (rest lst))) (encoding val (+ 1 count) (rest lst))]
    [else (append (list count) (encoding (first (rest lst)) 1 (rest lst)))]
    )
  )

(define (encode-with-lengths lst)
  (encoding 1 1 (remove-leading-zero lst) ))

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0))

#| d |#




(define (binary-odd? lst)
  (cond
    [(= 0 (first(reverse lst))) #f]
    [else #t]
    )
  )

(binary-odd? '(1 0 1 1 0)) 
(binary-odd? '(1 0 1 1 1)) 

#| e |#

(define (prev lst)
  (cond
    [(= 1 (length lst)) (list 0)]
    [(= 2 (length lst)) (cond [(= 0 (first lst)) (list 1)] [else (append (list 0) (first (rest lst)))])]
    [(= 0 (first lst)) (append (list 1) (prev (rest lst)))]
    [else (append (list 0) (rest lst))]
    )
  )

(define (decrement lst)
  (prev (reverse lst))
  )

(decrement '(1 0 1 1 0)) ; ==> '(1 0 1 0 1)
(decrement '(1 0 0 0 0)) ; ==> '(1 1 1 1)
(decrement '(0))

#| task 2|#

#| a |#



(define (alternating-sum lst)
  (cond
    [(empty? lst) 0]
    [(= 1 (length lst)) (first lst)]
    [else (+ (- (first lst) (first (rest lst))) (alternating-sum (rest (rest lst))))]
    )
  )

(alternating-sum (list 6 2 4 1 3 9))
(alternating-sum (list 1 2 3 4 5))

#| b 
(alternating-sum (list 1 2 3 4 5))
first step we execute 3rd statement and obtain this expression
=((1-2)+(alternating-sum (3 4 5))))
second step again 3rd statement 
=((1-2)+((3 - 4) + (alternating-sum (5)))))
last step we have one number then we call 2nd statement; just return this value after that simple calculation
=((1-2)+((3-4)+(5)))
=(1-2)+4
=3

|#
#| c |#

(define (alternating-sum-tail lst)
  (define (helper lst current)
  (cond
    [(empty? lst) current]
    [(= 1 (length lst)) (+ current (first lst))]
    [else (helper (rest (rest lst)) (+ current (- (first lst) (first (rest lst))) ))]
    )
  )
  (helper lst 0))

(alternating-sum-tail (list 6 2 4 1 3 9))
(alternating-sum-tail (list 1 2 3 4 5))

#| task 3|#


(define (dec n) (- n 1))
(define (f n)
(cond
[(<= n 2) (- 10 n)]
[else (* (f (dec (dec n))) (f (dec n)))]))

#|
(f 3)
-> if(3 <= 2) (10 - 3) else ((f (dec (dec 3))) * (f (dec 3)))
it's obvious that 3 not less than 2 hence second condition execute
-> ((f (dec (dec 3))) * (f (dec 3)))
above we can see dec function and calling (dec 3) = 2 
-> ((f (dec 2)) * (f 2))
dec(2) = 2 - 1 = 1
-> ((f 1)*(f 2))
here both by the condition both values 1 and 2 are les or equal to the 2 therefore we return (10 - n) statement
-> (10 - 1) * (10 - 2)
-> 9 * 8 = 72


|#