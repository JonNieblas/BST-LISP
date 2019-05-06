#lang racket
; got some help from these references:
; https://codereview.stackexchange.com/questions/206009/binary-search-tree-insertion-in-racket
; https://stackoverflow.com/questions/12850443/pre-in-and-post-order-of-an-arithmetic-tree-scheme-racket

; get root of tree
(define (root-node bst)
  (car bst))

; get left side of tree
(define (left-branch bst)
  (cadr bst))

; get right side of tree
(define (right-branch bst)
  (caddr bst))

; get subtrees of a node
(define (subtrees bst)
  (cdr bst))

; build bst from given list
(define (buildbst bst)
  (if (null? bst)
      null
      (add-nodes bst '())
  )
)

; add nodes into bst
(define (add-nodes bst tree)
  (if (null? bst)
      tree
      (add-nodes (subtrees bst) (descend-tree tree (root-node bst))) 
  )
)

; find spots for nodes
(define (descend-tree bst val)
  (cond
    ; create new node
    ((null? bst)(list val '() '()))
    ; if val < current root, descend left
    ((< val (root-node bst))
     (list (root-node bst) (descend-tree (left-branch bst) val) (right-branch bst)))
    ; if val > current root, descend right
    ((> val (root-node bst))
     (list (root-node bst)(left-branch bst)(descend-tree (right-branch bst) val)))
    ; in the scenario someone tries adding a duplicate, return self
    (else bst)
  )
)

; perform in order traversal on a given bst
(define (iot bst)
  (if (null? bst)
      null
      (append (iot (left-branch bst)) (list (root-node bst))(iot (right-branch bst))
      )
   )
)