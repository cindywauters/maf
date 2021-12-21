;; renamed lambdas/lets: 2
 
(define atom? (<change>
      (lambda (x)
         (not (pair? x)))
      (lambda (_x0)
         (not (pair? _x0)))))
 
(define VUB-circus (__toplevel_cons
      'ann
      (__toplevel_cons
         (__toplevel_cons
            'mien
            (__toplevel_cons
               (__toplevel_cons
                  'eef
                  (__toplevel_cons (__toplevel_cons 'bas ()) (__toplevel_cons (__toplevel_cons 'bob ()) ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'els
                     (__toplevel_cons (__toplevel_cons 'jan ()) (__toplevel_cons (__toplevel_cons 'jos ()) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'eva
                        (__toplevel_cons (__toplevel_cons 'tom ()) (__toplevel_cons (__toplevel_cons 'tim ()) ())))
                     ()))))
         (__toplevel_cons
            (__toplevel_cons
               'mies
               (__toplevel_cons
                  (__toplevel_cons
                     'ine
                     (__toplevel_cons (__toplevel_cons 'cas ()) (__toplevel_cons (__toplevel_cons 'cor ()) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'ils
                        (__toplevel_cons (__toplevel_cons 'rik ()) (__toplevel_cons (__toplevel_cons 'raf ()) ())))
                     (__toplevel_cons
                        (__toplevel_cons
                           'ines
                           (__toplevel_cons (__toplevel_cons 'stef ()) (__toplevel_cons (__toplevel_cons 'staf ()) ())))
                        ()))))
            ()))))
 
(define hoofdartiest (lambda (piramide)
      (car piramide)))
 
(define artiesten (lambda (piramide)
      (cdr piramide)))
 
(define artiest? (lambda (piramide)
      (if (pair? piramide) (atom? (car piramide)) #f)))
 
(define onderaan? (lambda (piramide)
      (null? (cdr piramide))))
 
(define jump (<change>
      (lambda (piramide artiest)
         (define jump-hulp (lambda (piramide pad)
               (if (if (artiest? piramide) (eq? (hoofdartiest piramide) artiest) #f)
                  pad
                  (jump-in (artiesten piramide) (cons (hoofdartiest piramide) pad)))))
         (define jump-in (lambda (lst pad)
               (if (null? lst)
                  #f
                  (let ((__or_res (jump-hulp (car lst) pad)))
                     (if __or_res __or_res (jump-in (cdr lst) pad))))))
         (reverse (jump-hulp piramide ())))
      (lambda (_piramide0 _artiest0)
         (define jump-hulp (lambda (_piramide1 _pad0)
               (if (if (artiest? _piramide1) (eq? (hoofdartiest _piramide1) _artiest0) #f)
                  _pad0
                  (jump-in (artiesten _piramide1) (cons (hoofdartiest _piramide1) _pad0)))))
         (define jump-in (lambda (_lst0 _pad1)
               (if (null? _lst0)
                  #f
                  (let ((___or_res0 (jump-hulp (car _lst0) _pad1)))
                     (if ___or_res0
                        ___or_res0
                        (jump-in (cdr _lst0) _pad1))))))
         (reverse (jump-hulp _piramide0 ())))))
 
(define fall (lambda (piramide artiest)
      (define fall-hulp (lambda (piramide pad)
            (if (if (artiest? piramide) (eq? (hoofdartiest piramide) artiest) #f)
               (append pad (list (hoofdartiest piramide)) (map hoofdartiest (artiesten piramide)))
               #f)
            (fall-in (artiesten piramide) (append pad (list (hoofdartiest piramide))))))
      (define fall-in (lambda (lst pad)
            (if (null? lst)
               #f
               (let ((__or_res (fall-hulp (car lst) pad)))
                  (if __or_res __or_res (fall-in (cdr lst) pad))))))
      (fall-hulp piramide ())))
 
(if (equal? (jump VUB-circus 'eva) (__toplevel_cons 'ann (__toplevel_cons 'mien ())))
   (if (equal? (jump VUB-circus 'stef) (__toplevel_cons 'ann (__toplevel_cons 'mies (__toplevel_cons 'ines ()))))
      (not
         (let ((__or_res (fall VUB-circus 'eva)))
            (if __or_res
               __or_res
               (let ((__or_res (fall VUB-circus 'stef)))
                  (if __or_res __or_res (fall VUB-circus 'mies))))))
      #f)
   #f)
 
