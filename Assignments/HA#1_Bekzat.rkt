#lang slideshow


; EX 1.1

; This function check given expression symbol letter or not
; expression will be always symbol cause before calling this function we check is this symbol or not in function variable in line 17
(define (isletter? expr)
  (or
   (and (symbol<? 'a expr) (symbol<? expr 'z)) ; checking that expression in range from 'a' to 'z' not included.
   (or (eq? 'a expr) (eq? 'z expr)) ; Due to the range not included, I additionally check the first and last letter
   )
  )

;this function check if this expression variable or not. First check that this symbol, after check that is letter
(define (variable? expr) 
  (and (symbol? expr) (isletter? expr))
  )

;for determine that this expression summatio, I just looking to first element in list, and if this symbol '+' then it's mean that this summation
;Also size of list should be greater than  cause expression can be “polyvariadic”.
(define (sum? expr)
  (and
   (and (list? expr) (< 2 (length expr)))
   (eq? (first expr) '+)
   )
  )

;this function help to determine that in expression not enough variables or numbers
; if expression not enough variables or number then function return true else false
(define (not-enough? expr x)
  (cond
    [(and (list? expr) (eq? (first expr) x)) (> 3 (length expr))] ; checking is it list and first element in list is  appropriate sumbol like '+' or '*'
    [else #f]
   )
  )


(define (summand-1 expr)
  (cond
    [(sum? expr)  (second expr)]
    [(product? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a product, not a sum" )]
    [(variable? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a variable, not a sum" )]
    [(number? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a constant, not a sum" )]
    [else (error "Expected a sum expression of the form (+ <expr> <expr>), but got: input is a function")]
  )
  )

(define (summand-2 expr)
  (cond
    [(sum? expr) (third expr)]
    [(product? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a product, not a sum" )]
    [(variable? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a variable, not a sum" )]
    [(not-enough? expr '+) (error "Expected a sum expression of the form (+ <expr> <expr>), sum does not have a second summand" )]
    [else (error "Expected a sum expression of the form (+ <expr> <expr>), but got: input is a function")]
   )
  )

; this function like sum but instead of '+' here wrote '*'
(define (product? expr)
  (and
   (list? expr)
   (eq? (first expr) '*)
   )
  )

(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [(sum? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a sum, not a product" )]
    [(variable? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a variable, not a produc" )]
    [(number? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a constant, not a product" )]
    [else (error "Expected a sum expression of the form (+ <expr> <expr>), but got: input is a function")]
    )
  )

(define (multiplier-2 expr)
  (cond
    [(product? expr) (third expr)]
    [(sum? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a sum, not a product" )]
    [(variable? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a variable, not a produc" )]
    [(number? expr) (error "Expected a sum expression of the form (+ <expr> <expr>), but got: a constant, not a product" )]
    [(not-enough? expr '*) (error "Expected a sum expression of the form (+ <expr> <expr>), sum does not have a second multiplier" )]
    [else (error "Expected a sum expression of the form (+ <expr> <expr>), but got: input is a function")]
   )
  )


; ex1.2
; derivatives codes start 

; this function get expression and variable. After it's calculate the derivative where contain operation product and return result
; in expression may contain some list which need to be take derivative therefore I use map to going through each elemnt.
; We take derivative from current and multiply to each other. To multiply each other I use again map too  
;by multiplication formula, example:(uvw)'= u'vw+uv'w+uvw'
(define (multi expr var)
  (cons
   '+
   (map
    (lambda (take_derivative)
      ( cons '* (map
                 (lambda (parts)(cond[(eq? take_derivative parts) (derivative parts var)][else parts] ) )
                 expr)
             )
      )
    expr)
  )
)

; I just use the chain rule: (f(x)^constant)'= f(x)'*constant*(f(x)^(constant))
; as you can see here no some exception, because before calling this function I already checked the valid of expression 
; expr = f(x) ^ g(x) -> (f(x) ^ g(x))' = g(x)*f(x)^(g(x)-1)*f(x)' + f(x)^g(x)*ln(f(x))*g(x)'
(define (exponent-derivative expr x)
  (list '+
        (list '*
        (derivative (second expr) x) ; (f(x)')
        (list '*
          (third expr) ; constant
          (list '^
            (second expr) ; f(x)
            (list '- (third expr) 1) ;constant-1
            )
         )
        )
        (list '*
              (derivative (third expr) x)
              (list '*
                    (list 'ln (second expr))
                    (list '^
                          (second expr)
                          (third expr)
                          )
                    )
              )
  )
)

; this function which calculate derivatives.
; almost everywhere used chain rule in derivative
(define (derivative expr x)
  (cond
    ; In in first case expr = (log f(x)) -> log(f(x))' = (f(x)')/(ln(10)*f(x)) here again use chain rule
    [(log? expr) (list '/ (derivative (second expr) x) (list '* (list  'ln '10) (second expr)))]
    
    ;In second case expr = (sin f(x)) -> sin(f(x))' = f(x)'cos(f(x))
    [(sin? expr) (list '* (derivative (second expr) x) (list 'cos (second expr)))]

    [(exponent? expr) (exponent-derivative expr x)] ; just call another function
    ; same as sin function ; expr = cos(f(x)) -> cos(f(x))' = f(x)'*-1*sin(f(x))

    [(cos? expr) (list '* (derivative (second expr) x) '- (list 'sin (second expr)))]
    ; just use formula; expr = tan(f(x)) ->  (tan(f(x)))' = f(x)'/cos(f(x))^2

    [(tan? expr) (list '/ (derivative (second expr) x) (list '^ (list 'cos (second expr)) '2))]
    ; here in addition we don't know number of element therefore I used map. Using map I take derivative in each elemnt and adding all of them  

    [(sum? expr) (cons '+ (map (lambda(lst) (derivative lst x)) (rest expr)))] 

    [(product? expr)  (multi (rest expr) x) ] ; just call another function

    [(eq? expr x) '1]; simple case when we have one variable x

    [else 0] ; in other case return 0
   )
  )


; derivatives codes ends 
;ex 1.3


(define (isZero? x)
  (and (number? x) (= x 0))
  )

(define exist-variable
  (lambda(x)(or (list? x) (variable? x)))
  )


;all simplifys function are get expression which not contain not simplified sub exrpression
; which mean we haven't get this expression: (+ 4 (+ 6 5)), We get thie: (+ 4 11)
; as you can see all subexpression is simplified



(define (add expr)
  (cond
    ; first case when in our expression hasn't variables then whe just return some of the number
    ; to determine presence of some variable, I use filter function where check if expression contain list or variables 
    [(empty? (filter exist-variable expr)) (foldl + 0 (filter number? expr))]

    ; second case when sum of number equal to zero or expression doesn't contain any numbers
    ; then return list of variables 
    [(or (empty? (filter number? expr)) (= 0 (foldl + 0 (filter number? expr))))
     (cond
       ; also check if number of element equal to 1 then no need to write symbol '+'
       ; otherwise we just add symbol '+' at beggining 
       [(= 1 (length (filter exist-variable expr))) (filter exist-variable expr)]
       [else (cons '+ (filter exist-variable expr))]
       ) ]
    ; last case we just combine two list
    [else (append (list '+ (foldl + 0 (filter number? expr))) (filter exist-variable expr))]
 
   )
  )


; function to simplify multiplied expression 
(define (multiply expr)
  (cond
    ; first case if expression contain 0 then answer will be 0
    [(ormap isZero? expr) 0]

    ; this function same as add function 
    [(empty? (filter exist-variable expr)) (foldl * 1 (filter number? expr))]
    [(or (empty? (filter number? expr)) (= 1 (foldl * 1 (filter number? expr))))
     (cond
       [(= 1 (length (filter exist-variable expr))) (filter exist-variable expr)]
       [else (cons '* (filter exist-variable expr))]
       )]
    [else  (append (list '* (foldl * 1 (filter number? expr))) (filter exist-variable expr))]
   
   ))

;this function simplify the exponent
(define (exponent_simpl num pow)
  (cond
    [(and (number? pow) (= 0 pow)) 1] ; for all case if exponent is zero then answer always will be 1
    [(and (number? num) (number? pow)) (* num (exponent_simpl num (- pow 1)))] ; second case if num and pow is numbers then call recursive function to calculate
    [else (list '^ num pow)] ; otherwise return list 
    )
  )

; function for tan, cos, sin, log almost same
; check if the val variable is number or not; if yes then call function
; otherwise return list 
(define (log_simpl val)
  (cond
    [(number? val) (log val)]
    [else (list 'log val)]
    )
  )

(define (sin_simpl val)
  (cond
    [(number? val) (sin val)]
    [else (list 'sin val)]
    )
  )


(define (cos_simpl val)
  (cond
    [(number? val) (cos val)]
    [else (list 'cos val)]
    )
  )

(define (tan_simpl val)
  (cond
    [(number? val) (tan val)]
    [else (list 'tan val)]
    )
  )


(define (div_simpl num denom)
  (cond
    [(and (number? denom) (= 0 denom)) (error "Impossible to divide to the 0" )]
    [(and (number? num) (number? denom)) (/ num denom)]
    [else (list '/ num denom)]
    )
  )

(define (minus_simpl a b)
  (cond
    [(and (number? a) (number? b)) (- a b)]
    [else (list '- a b)]
    )
  )

(define (rmv-bracket expr)
  (cond
    [(and (list? expr)(= 1 (length expr))) (rmv-bracket (first expr))]
    [else expr])
  )

; function which simplify the expression, for every case we call other function
(define (simplify expr)
  (cond
    [(log? expr) (rmv-bracket (log_simpl (simplify (second expr))))]
    [(sin? expr) (rmv-bracket (sin_simpl (simplify (second expr))))]
    [(cos? expr) (rmv-bracket (cos_simpl (simplify (second expr))))]
    [(tan? expr) (rmv-bracket (tan_simpl (simplify (second expr))))]
    [(exponent? expr) (rmv-bracket (exponent_simpl (simplify (second expr)) (simplify (third expr))))]
    [(div? expr) (rmv-bracket (div_simpl (simplify (second expr)) (simplify (third expr))))]
    [(minus? expr) (rmv-bracket (minus_simpl (simplify (second expr)) (simplify (third expr))))]
    ; in sum and product I use map, because number of elemtns in list unknowm and  I need put simplified expresion to another function.   
    [(sum? expr)   (rmv-bracket (add (map (lambda (x) (simplify x)) (rest expr)))) ] 
    [(product? expr) (rmv-bracket (multiply (map (lambda (x)  (simplify x)) (rest expr))))]
    [else (rmv-bracket expr)]
    )
  )

;ex 1.5

; this function convert to infix form
; here expr = (symbol first second); first and second can be also  some expression, hence I call the infix for first and second
; if expr is product or sum then return list where first and symbol positions are swapped. 
(define (to-infix expr)
  (cond
    [(or (sum? expr) (product? expr))
     (list
      (to-infix (second expr))
      (to-infix (first expr))
      (to-infix (third expr))
      )]
    [else expr])
  )

;ex1.6

; function in below almost all same. Just checking length, number of element and symbols
(define (exponent? expr)
  (and
   (and (list? expr) (= 3 (length expr)))
   (and (list? expr) (eq? (first expr) '^))
   )
  )

(define (sin? expr)
  (and
  (and (list? expr) (= 2 (length expr)))
  (and (list? expr) (eq? (first expr) 'sin))
  )
  )


(define (cos? expr)
  (and
  (and (list? expr) (= 2 (length expr)))
  (and (list? expr) (eq? (first expr) 'cos))
  )
  )


(define (tan? expr)
  (and
  (and (list? expr) (= 2 (length expr)))
  (and (list? expr) (eq? (first expr) 'tan))
  )
)

(define (log? expr)
  (and
  (and (list? expr) (= 2 (length expr)))
  (and (list? expr) (eq? (first expr) 'log))
  )
  )

(define (div? expr)
  (and
   (and (list? expr) (= 3 (length expr)))
   (and (list? expr) (eq? (first expr) '/))
   )
  )

(define (minus? expr)
  (and
   (and (list? expr) (= 3 (length expr)))
   (and (list? expr) (eq? (first expr) '-))
   )
  )

;ex1.7

;ex1.8

; cur is the current element from out expression, result is list where contain out final results
(define (get-variables cur result)
  (cond 
    [(list? cur) (foldl get-variables result cur)] ; first checking if cur is list then we need add elements from cur to result. To do this I call foldl. It's mean that is the recursive 
    [(variable? cur) ; if current element is variable then return list with result and cur element
     (append ; combine lists in below and return
      (filter (lambda (x) (symbol<? x cur) ) result) ; get list from result which less than cur element
      (list cur) ; put cur element
      (filter (lambda (x) (symbol<? cur x)) result) ; get list from result which greater than cur elemnt
      )
     ]
    [else result]
    )
)

;using functions foldl and get-variables to get sorted distinct variables 
(define (variables-of lst) (foldl get-variables empty lst))

;ex1.9

; just using rule gradient, just taking derivative from expression for each element in list lst   
(define (gradient expr lst)
  (map (lambda (x) ( simplify (derivative expr x))) lst)
  )
