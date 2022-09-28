#lang slideshow

;ex1 

;a
(define (replicate n txt)
  (cond
    [(= 0 n) empty]
    [else (cons txt (replicate (- n 1) txt))]
    )
  )

(replicate 10 'a)
(replicate 3 '(1 . 2))

; b
(define (prefix lst x)
  (cond
    [(or (> 1 x) (empty? lst)) empty]
    [else (append (list (first lst)) (prefix (rest lst) (- x 1)))]
    )
  )

; done without dots
(define (split lst x)
  (cons
   (prefix lst x)
   (list (reverse (prefix (reverse lst) (- (length lst) x))))
   )
  )

(split '(1 2 3 4 5) 2)
(split '(a b c d) 4)
(split '(a b c) 4)
(split '(a b c) 0)

; c

(define (chunks lst x)
  (cond
    [(<= (length lst) x) (list lst)]
    [else
     (cons
      (prefix lst x)
      (chunks
       (second (split lst x))
       x)
      )]
    )
  )

(chunks '(1 2 3 4 5) 2)
(chunks '(a b c d e f) 3)

;d

(define (windows lst x)
  (cond
    [(<= (length lst) x) (list lst)]
    [else
     (cons (prefix lst x) (windows (rest lst) x))
     ]
   )
  )

(windows '(1 2 3 4 5) 2)
(windows '(a b c d e) 3)

; ex2

(define (del lst)
  (filter (lambda (y) (not (empty? y))) lst)
  )

; I use function reverse to save order
(define (pairs lst)
  (reverse (foldl append empty (map (
        lambda (x)
         (
          del (reverse (map
          (lambda (y)
            (cond[(symbol<? x y) (cons x y)][empty]))
          lst
          )))
        ) lst)))
  
  )


(pairs '(a b c d))

; b

(define (splits lst)
  (map
   (lambda (c)
           (split lst c)
           )
   (inclusive-range 0 (length lst))
         )
  )

(splits '(a b c))

;d

(define (mult-pair p cm) (cm (car p) (cdr p)))

(define (max-with lst x cm)
  (
      foldl
      (lambda(val answer)
        (cond
         [(= x val) answer]
         [(or (empty? answer) (< (mult-pair answer cm) (cm val x))) (cons x val)]
         [else answer]
         )
       )
      empty
     lst
     )
  )

(define (max-binary-op cm lst)
  (foldl
   (lambda (a result)
     (cond
       [(empty? result) (max-with lst a cm)]
       [(< (mult-pair result cm) (mult-pair (max-with lst a cm) cm)) (max-with lst a cm)]
       [else result]
       )
    )
   empty
   lst)
  )

(max-binary-op * '(1 2 3 4 3 2 1))
(max-binary-op - '(1 2 3 4 3 2 1))

;c


(define (max-product lst)
  (max-binary-op * lst)
  )

(max-product '(1 2 3 4 3 2 1))
(max-product '(31 4 42  32 23 2))

;e 

(define (sorted? lst)
  (foldl
   (lambda (a result)
     (cond
       [(eq? #f result) #f]
       [(symbol<? result a) a]
       [else #f]
       ))
   (first lst)
   (rest lst))
  )


(define (combinations lst n)
  (filter (lambda (x) (not (eq? #f (sorted? x)))) (map (lambda (x) (prefix x n)) (permutations lst)))
  )

(combinations '(a b c d) 3)

; 3
;a
(define (max lst)
  (foldl (lambda (a b) (if (> a b ) a b)) (first lst) (rest lst))
  )


(define (second-max lst)
  (car (foldl
   (lambda (a result)
     (cond
       [(not (pair? result)) (cond[(< a result) (cons a result)][else (cons result a)])]
       [(< (cdr result) a) (cons (cdr result) a)]
       [(< (car result) a) (cons a (cdr result))]
       [else result]
       ))
   (first lst) (rest lst)) 
  ))


(define (top-3 lst)
  (foldl
   (lambda (a result)
     (cond
       [(= 1 (length result)) (cond[(< a (first result))
                                    (append (list a) result)][else (append result (list a))])]
       [(= 2 (length result))
        (cond
          [(> a (first (rest result))) (append result (list a))]
          [(> a (first result)) (append (list (first result)) (list a) (list (first (rest result))))]
          [else (append (list a) result)]
          )
        ]
       [(> a (first (rest (rest result)))) (append (rest result) (list a))]
       [(> a (first (rest result))) (append (list (first (rest result))) (list a) (rest (rest result)))]
       [(> a (first result)) (append (list a) (rest result))]
       [else result]
       ))
   (list (first lst)) (rest lst)) 
  )


(max '(1 5 3 6 2 0))
(second-max '(1 5 3 6 2 0))
(top-3 '(5 3 6 2 8 1 0))

;d

(define (group lst)
  (reverse (foldl
   (lambda (a result)
     (cond
      [(empty? result) (list (list a))]
      [(eq? a (first (first result)))  (append (list (append (list a) (first result))) (rest result))]
      [else  (append (list (list a)) result)]
      )
    )
   empty
   lst
   ))
  )

(group '(a b b c c c b a a))


;e

(define (cumulative-sums lst)
  (reverse (foldl
   (lambda(a result)
      (cond
        [(empty? result) (list a 0)]
        [else (append (list (+ a (first result))) result)]
       )
          )
   empty
   lst))
  )

(cumulative-sums '(1 2 3 4 5))
