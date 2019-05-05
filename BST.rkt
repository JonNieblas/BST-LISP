#lang racket
(define (lower a x)
  (if (null? x)
    null
    (if (< (car x) a)
       (cons (car x) (lower a (cdr x)) )
       (lower a (cdr x))
    )
  )
)

(define (app a x)
  (if(null? x)
     (cons a x)
     (cons (car x) (app a (cdr x)))
   )
)

(define (ins a p x)
  (if (= p 0)
      (if (null? x)
          (cons a x)
          (cons a (ins a (- p 1) (cdr x)))
      )
      (if (null? x)
          null
          (cons (car x) (ins a (- p 1) (cdr x)))
      )
   )
)

; test: (iot '(3 (1 () ()) (7 (5 () ()) ()) ) )
(define (iot bst)
  (if (null? bst)
      (println "null")
      (if (null? (cdr bst))
          null
          (if (null? (car (cdr bst)))
              (if (null? (cdr (cdr bst)))
                  null
                  (println (car bst))
                  ;(cons (car bst) (iot (cdr (cdr bst))))
                  )
              (iot (car (cdr bst)))
          )
      )
   )
  (println "made it")
 )

(define (iot2 bst)
  (if (null? bst)
      null
      (check-both bst)
   )
)

;function that checks both trees for null
(define (check-both bst)
  (if (null? (car (cdr bst)))
      (cons (car bst) '())
      (cons (check-both (car (cdr bst))) (car bst))
   )
  (if (null? (car (cdr (cdr bst))))
      null
      (cons (car bst) (check-both (car (cdr (cdr bst)))))
  )
)