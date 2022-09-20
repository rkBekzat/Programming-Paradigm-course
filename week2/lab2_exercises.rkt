#lang slideshow 


(define (render-bit x)
  (cond
    [(= x 1) (filled-rectangle 10 20)]
    [else (rectangle 10 20)]))


(define (render-bits lst)
  (cond
    [(empty? lst) (blank)]
    [else (hc-append
           (render-bit (first lst)) (render-bits (rest lst)))
          ]))


(define (count-ones lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (count-ones (rest lst)))]
    ))



(define (prefix-zero lst)
  (cond
    [(empty? lst) 0]
    [(= 1 (first lst)) 0 ]
    [else (+ 1 (prefix-zero (rest lst)))]
    ))

(define (trailing-zeros lst)
  (prefix-zero (reverse lst)))

(define (add-one lst)
  (cond
    [(empty? lst) (list 1)]
    [(= 1 (first lst)) (append (list 0) (add-one (rest lst)))]
    [else (append (list 1) (rest lst))]
    ))

(define (increment lst)
  (reverse (add-one (reverse lst))))

(define (increments n lst)
  (cond
    [(= 1 n) ( list lst)]
    [else (cons lst (increments (- n 1) (increment lst)))]
    ))


(define (render-many-bits lsts)
  (cond
    [(= 1 (length lsts)) (render-bits (first lsts))]
    [else (vr-append (render-bits (first lsts)) (render-many-bits (rest lsts)))]
    ))