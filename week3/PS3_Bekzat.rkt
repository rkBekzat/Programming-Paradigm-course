#lang slideshow

; ex 1 (a)


(define (binary-to-decimal lst)
  (foldl
   (lambda (a b result)
     (+ result (* a b)))
   0
   (map (lambda (x) (expt 2 x)) (range (length lst)))
   (reverse lst)) 
)

(binary-to-decimal '(1 0 1 1 0))

(define (to-list lst)
  (cond
    [(list? lst) lst]
    [else (list lst)]
    )
  )

(define (remove-leading-zeros lst)
  (reverse (foldl
   (lambda (bit result)
     (cond
       [(empty? result) (cond[(= 1 bit) 1][else empty])]
       [else
        (append (to-list bit) (to-list result))]
       ))
   '() lst))
  )

(remove-leading-zeros '(0 0 0 1 0 1 1 0))


(define (count-zeros lst)
  (foldl
   (lambda (a result)
     (+ a (- 1 result))
     )
   0
   (remove-leading-zeros lst))
  )

(count-zeros '(0 0 0 1 0 1 1 0))

;(define (group-consecutive lst)
 ; (foldl
  ; (lambda (a b result)
  ; ())
  ; empty
  ; lst
  ; (rest lst)
  ; )
  ;)

; ex2

; a
(define (fullname name)
  (cons (car name) (car (cdr name)))
  )

(fullname '("John" "Malkovich" . 29))

(define (same-name name)
  (lambda (x) (eq? name (car x)))
  )

(define (employees lst)
  (filter (same-name "Anna") lst)
  )

(employees
'(("John" "Malkovich" . 29)
("Anna" "Petrova" . 22)
("Ivan" "Ivanov" . 23)
("Anna" "Karenina" . 40)))


(define (employees-over-25 lst)
  (map fullname (filter (lambda (x) (< 25 (cdr (cdr x)))) lst))
  )

(employees-over-25
'(("John" "Malkovich" . 29)
("Anna" "Petrova" . 22)
("Ivan" "Ivanov" . 23)
("Anna" "Karenina" . 40)))
; ex3
