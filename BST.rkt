#lang racket
; help from: https://stackoverflow.com/questions/12850443/pre-in-and-post-order-of-an-arithmetic-tree-scheme-racket

; get root of tree
(define (root-node tree)
  (car tree))

; get left side of tree
(define (left-branch tree)
  (cadr tree))

; get right side of tree
(define (right-branch tree)
  (caddr tree))

; go down left subtree first,
; return to root when left tree returns null,
; then go down right tree last
(define (iot tree)
  (if (null? tree)
      '()
      (append (iot (left-branch tree)) (list (root-node tree))(iot (right-branch tree))
      )
   )
)