;; renamed lambdas/lets: 5
 
(define make-node (lambda (key left right parent info)
      (list key left right parent info)))
 
(define key (lambda (node)
      (list-ref node 0)))
 
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
 
(define info (<change>
      (lambda (node)
         (list-ref node 4))
      (lambda (_node0)
         (list-ref _node0 4))))
 
(define set-key! (lambda (node key)
      (set-car! node key)))
 
(define set-left! (lambda (node l)
      (set-car! (cdr node) l)))
 
(define set-right! (<change>
      (lambda (node r)
         (set-car! (cddr node) r))
      (lambda (_node0 _r0)
         (set-car! (cddr _node0) _r0))))
 
(define set-parent! (lambda (node p)
      (set-car! (cdddr node) p)))
 
(define set-info! (lambda (node i)
      (set-car! (cddddr node) i)))
 
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
 
