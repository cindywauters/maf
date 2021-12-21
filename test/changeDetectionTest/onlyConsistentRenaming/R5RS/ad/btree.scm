;; renamed lambdas/lets: 8
 
(define make-node (<change>
      (lambda (key left right parent info)
         (list key left right parent info))
      (lambda (_key0 _left0 _right0 _parent0 _info0)
         (list _key0 _left0 _right0 _parent0 _info0))))
 
(define key (<change>
      (lambda (node)
         (list-ref node 0))
      (lambda (_node0)
         (list-ref _node0 0))))
 
(define left (<change>
      (lambda (node)
         (list-ref node 1))
      (lambda (_node0)
         (list-ref _node0 1))))
 
(define right (<change>
      (lambda (node)
         (list-ref node 2))
      (lambda (_node0)
         (list-ref _node0 2))))
 
(define parent (lambda (node)
      (list-ref node 3)))
 
(define info (lambda (node)
      (list-ref node 4)))
 
(define set-key! (<change>
      (lambda (node key)
         (set-car! node key))
      (lambda (_node0 _key0)
         (set-car! _node0 _key0))))
 
(define set-left! (lambda (node l)
      (set-car! (cdr node) l)))
 
(define set-right! (lambda (node r)
      (set-car! (cddr node) r)))
 
(define set-parent! (<change>
      (lambda (node p)
         (set-car! (cdddr node) p))
      (lambda (_node0 _p0)
         (set-car! (cdddr _node0) _p0))))
 
(define set-info! (<change>
      (lambda (node i)
         (set-car! (cddddr node) i))
      (lambda (_node0 _i0)
         (set-car! (cddddr _node0) _i0))))
 
(define null-tree ())
 
(define null-tree? (lambda (tree)
      (null? tree)))
 
(define is-leaf? (<change>
      (lambda (node)
         (if (null-tree? (left node))
            (null-tree? (right node))
            #f))
      (lambda (_node0)
         (if (null-tree? (left _node0))
            (null-tree? (right _node0))
            #f))))
 
(define is-root? (lambda (node)
      (null-tree? (parent node))))
 
