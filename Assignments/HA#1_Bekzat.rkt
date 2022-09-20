#lang slideshow


; EX 1.1

(define (isletter? expr)
  (or
   (and (symbol<? 'a expr) (symbol<? expr 'z))
   (or (eq? 'a expr) (eq? 'z expr))
   )
  )

(define (variable? expr)
  (and (symbol? expr) (isletter? expr))
  )


(define (sum? expr)
  (and
   (list? expr)
   (eq? (first expr) '+)
   )
  )

(define (summand-1 expr)
  (cond
    [(sum? expr)  (second expr)]
    [else "this not sum"]
  )
  )

(define (summand-2 expr)
  (cond
    [(sum? expr) (third expr)]
    [else "this not sum"]
   )
  )

(define (product? expr)
  (and
   (list? expr)
   (eq? (first expr) '*)
   )
  )

(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else "this not multiplication"]
    )
  )

(define (multiplier-2 expr)
  (cond
    [(product? expr) (third expr)]
    [else "this not multiplication"]
   )
  )


; ex1.2

; derivatives codes start 
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

(define (derivative expr x)
  (cond
    [(log? expr) ]
    [(sin? expr) (list '* (derivative expr x) (list 'cos expr))]
    [(exponent? expr) (list '* (third expr) (derivative (second expr)))]
    [(cos? expr) (list '* (derivative expr x) '- (list 'sin expr))]
    [(tan? expr)]
    [(sum? expr) (cons '+ (map (lambda(lst) (derivative lst x)) (rest expr)))]
    [(product? expr)  (multi (rest expr) x) ]
    [(eq? expr x) '1]
    [else 0]
   )
  )


; derivatives codes ends 
;ex 1.3

(define (number? expr)
  (and (not (symbol? expr)) (not (list? expr)))
  )


(define (add expr1 expr2)
  (cond
    [(eq? 0 expr1) expr2]
    [(eq? 0 expr2) expr1]
    [(or (variable? expr1)(list? expr1)) (list '+ expr1 expr2)]
    [(or (variable? expr2)(list? expr2)) (list '+ expr1 expr2)]
    [else (+ expr1 expr2)]
   )
  )

(define (multiply expr1 expr2)
  (cond
    [(or (eq? 0 expr1) (eq? 0 expr2)) '0]
    [(eq? 1 expr1) expr2]
    [(eq? 1 expr2) expr1]
    [(or (variable? expr1)(list? expr1)) (list '*  expr1 expr2)]
    [(or (variable? expr2)(list? expr2)) (list '*  expr1 expr2)]
    [else (* expr1 expr2)]
   ))

(define (simplify expr)
  (cond
    [(sum? expr) (add (simplify (second expr)) (simplify (third expr))) ]
    [(product? expr) (multiply (simplify (second expr)) (simplify (third expr)))]
    [else expr]
    )
  )

;ex 1.5

(define (to-infix expr)
  (cond
    [(or (sum? expr) (product? expr)) (list (to-infix (second expr)) (to-infix (first expr)) (to-infix (third expr)))]
    [else expr])
  )

;ex1.6

(define (exponent? expr)
  (and (list? expr) (eq? (first expr) '^))
  )

(define (sin? expr)
  (and (list? expr) (eq? (first expr) 'sin))
  )


(define (cos? expr)
  (and (list? expr) (eq? (first expr) 'cos))
  )


(define (tan? expr)
  (and (list? expr) (eq? (first expr) 'tan))
  )


(define (log? expr)
  (and (list? expr) (eq? (first expr) 'log))
  )



;ex1.7

;ex1.8

;ex1.9