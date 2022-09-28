#lang slideshow

(define (test lst n)
  (foldl
   (lambda (val result)
     (cond
       [(empty? result) (list val)]
       [(<= n (length result)) result]
       [else (append result (list val))]
       )
     )
   empty
   lst)
  )

(test '(a b c d e r) 3)