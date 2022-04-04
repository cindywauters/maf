;; renamed lambdas/lets: 11
;; Of which consistent renamings: 9

(define apply-append (lambda (args)
      (if (null? args)
         ()
         (if (null? (cdr args))
            (car args)
            (if (null? (cddr args))
               (append (car args) (cadr args))
               (if (null? (cdddr args))
                  (append (car args) (append (cadr args) (caddr args)))
                  (error "apply-append" args)))))))
 
(define lexico (lambda (base)
      (define lex-fixed (lambda (fixed lhs rhs)
            (define check (<change>
                  (lambda (lhs rhs)
                     (if (null? lhs)
                        fixed
                        (let ((probe (base (car lhs) (car rhs))))
                           (if (let ((__or_res (eq? probe 'equal))) (if __or_res __or_res (eq? probe fixed)))
                              (check (cdr lhs) (cdr rhs))
                              'uncomparable))))
                  (lambda (_lhs0 _rhs0)
                     (if (null? _lhs0)
                        fixed
                        (let ((_probe0 (base (car _lhs0) (car _rhs0))))
                           (if (let ((___or_res0 (eq? _probe0 'equal))) (if ___or_res0 ___or_res0 (eq? _probe0 fixed)))
                              (check (cdr _lhs0) (cdr _rhs0))
                              'uncomparable))))))
            (check lhs rhs)))
      (define lex-first (<change>
            (lambda (lhs rhs)
               (if (null? lhs)
                  'equal
                  (let ((probe (base (car lhs) (car rhs))))
                     (if (let ((__or_res (eq? probe 'less))) (if __or_res __or_res (eq? probe 'more)))
                        (lex-fixed probe (cdr lhs) (cdr rhs))
                        (if (eq? probe 'equal)
                           (lex-first (cdr lhs) (cdr rhs))
                           (if (eq? probe 'uncomparable) 'uncomparable #f))))))
            (lambda (_lhs0 _rhs0)
               (if (equal? _lhs0 _rhs0) ;; NOT RENAMING, changed from null? _lhs0
                  'equal
                  (let ((_probe0 (base (car _lhs0) (car _rhs0))))
                     (if (let ((___or_res0 (eq? _probe0 'less))) (if ___or_res0 ___or_res0 (eq? _probe0 'more)))
                        (lex-fixed _probe0 (cdr _lhs0) (cdr _rhs0))
                        (if (eq? _probe0 'equal)
                           (lex-first (cdr _lhs0) (cdr _rhs0))
                           (if (eq? _probe0 'uncomparable) 'uncomparable #f))))))))
      lex-first))
 
(define make-lattice (<change>
      (lambda (elem-list cmp-func)
         (cons elem-list cmp-func))
      (lambda (_elem-list0 _cmp-func0)
         (cons _elem-list0 _cmp-func0))))
 
(define lattice->elements car)
 
(define lattice->cmp cdr)
 
(define zulu-select (lambda (test lst)
      (define select-a (<change>
            (lambda (ac lst)
               (if (null? lst)
                  (reverse! ac)
                  (select-a (let ((head (car lst))) (if (test head) (cons head ac) ac)) (cdr lst))))
            (lambda (_ac0 _lst0)
               (if (null? _lst0)
                  (reverse! _ac0)
                  (select-a (let ((_head0 (car _lst0))) (if (test _head0) (cons _head0 _ac0) _ac0)) (cdr _lst0))))))
      (select-a () lst)))
 
(define reverse! (<change>
      (letrec ((rotate (lambda (fo fum)
                         (let ((next (cdr fo)))
                            (set-cdr! fo fum)
                            (if (null? next) fo (rotate next fo))))))
         (lambda (lst)
            (if (null? lst) () (rotate lst ()))))
      (letrec ((_rotate0 (lambda (_fo0 _fum0)
                           (let ((_next0 (cdr _fo0)))
                              (set-cdr! _fo0 _fum0)
                              (if (null? _next0) _fo0 (_rotate0 _next0 _fo0))))))
         (lambda (_lst0)
            (if (null? _lst0) () (_rotate0 _lst0 ()))))))
 
(define select-map (<change>
      (lambda (test func lst)
         (define select-a (lambda (ac lst)
               (if (null? lst)
                  (reverse! ac)
                  (select-a (let ((head (car lst))) (if (test head) (cons (func head) ac) ac)) (cdr lst)))))
         (select-a () lst))
      (lambda (_test0 _func0 _lst0)
         (define select-a (lambda (_ac0 _lst1)
               (if (null? _lst1)
                  (reverse! _ac0)
                  (select-a
                     (let ((_head0 (car _lst1)))
                        (if (_test0 _head0)
                           (cons (_func0 _head0) _ac0)
                           _ac0))
                     (cdr _lst1)))))
         (select-a () _lst0))))
 
(define map-and (lambda (proc lst)
      (if (null? lst)
         #t
         (<change>
            (letrec ((drudge (lambda (lst)
                               (let ((rest (cdr lst)))
                                  (if (null? rest)
                                     (proc (car lst))
                                     (if (proc (car lst)) (drudge rest) #f))))))
               (drudge lst))
            (letrec ((_drudge0 (lambda (_lst0)
                                 (let ((_rest0 (cdr _lst0)))
                                    (if (null? _rest0)
                                       (proc (car _lst0))
                                       (if (proc (car _lst0)) (_drudge0 _rest0) #f))))))
               (_drudge0 lst))))))
 
(define maps-1 (<change>
      (lambda (source target pas new)
         (let ((scmp (lattice->cmp source))
               (tcmp (lattice->cmp target)))
            (let ((less (select-map (lambda (p) (eq? 'less (scmp (car p) new))) cdr pas))
                  (more (select-map (lambda (p) (eq? 'more (scmp (car p) new))) cdr pas)))
               (zulu-select
                  (lambda (t)
                     (if (map-and (lambda (t2) (memq (tcmp t2 t) (__toplevel_cons 'less (__toplevel_cons 'equal ())))) less)
                        (map-and (lambda (t2) (memq (tcmp t2 t) (__toplevel_cons 'more (__toplevel_cons 'equal ())))) more)
                        #f))
                  (lattice->elements target)))))
      (lambda (_source0 _target0 _pas0 _new0)
         (let ((_scmp0 (lattice->cmp _source0))
               (_tcmp0 (lattice->cmp _target0)))
            (let ((_less0 (select-map (lambda (_p0) (eq? 'less (_scmp0 (car _p0) _new0))) cdr _pas0))
                  (_more0 (select-map (lambda (_p1) (eq? 'more (_scmp0 (car _p1) _new0))) cdr _pas0)))
               (zulu-select
                  (lambda (_t0)
                     (if (map-and (lambda (_t20) (memq (_tcmp0 _t20 _t0) (__toplevel_cons 'less (__toplevel_cons 'equal ())))) _less0)
                        (map-and
                           (lambda (_t21)
                              (memq (_tcmp0 _t21 _t0) (__toplevel_cons 'more (__toplevel_cons 'equal ()))))
                           _more0)
                        #f))
                  (lattice->elements _target0)))))))
 
(define maps-rest (lambda (source target pas rest to-1 to-collect)
      (if (null? rest)
         (to-1 pas)
         (<change>
            (let ((next (car rest))
                  (rest (cdr rest)))
               (to-collect
                  (map
                     (lambda (x)
                        (maps-rest source target (cons (cons next x) pas) rest to-1 to-collect))
                     (maps-1 source target pas next))))
            (let ((_rest0 (cdr rest)) ;; NOT RENAMING, swapped these two around
                  (_next0 (car rest)))
               (to-collect
                  (map
                     (lambda (_x0)
                        (maps-rest source target (cons (cons _next0 _x0) pas) _rest0 to-1 to-collect))
                     (maps-1 source target pas _next0))))))))
 
(define maps (lambda (source target)
      (make-lattice
         (maps-rest
            source
            target
            ()
            (lattice->elements source)
            (lambda (x)
               (list (map cdr x)))
            (lambda (x)
               (apply-append x)))
         (lexico (lattice->cmp target)))))
 
(define count-maps (lambda (source target)
      (maps-rest
         source
         target
         ()
         (lattice->elements source)
         (lambda (x) 1)
         sum)))
 
(define sum (lambda (lst)
      (if (null? lst) 0 (+ (car lst) (sum (cdr lst))))))
 
(define run (<change>
      (lambda ()
         (let* ((l2 (make-lattice
                      (__toplevel_cons 'low (__toplevel_cons 'high ()))
                      (lambda (lhs rhs)
                         (if (eq? lhs 'low)
                            (if (eq? rhs 'low)
                               'equal
                               (if (eq? rhs 'high)
                                  'less
                                  (error "make-lattice base")))
                            (if (eq? lhs 'high)
                               (if (eq? rhs 'low)
                                  'more
                                  (if (eq? rhs 'high)
                                     'equal
                                     (error "make-lattice base")))
                               (error "make-lattice base"))))))
                (l3 (maps l2 l2))
                (l4 (maps l3 l3)))
            (count-maps l2 l2)
            (count-maps l3 l3)
            (count-maps l2 l3)
            (count-maps l3 l2)
            (count-maps l4 l4)))
      (lambda ()
         (let* ((_l20 (make-lattice
                        (__toplevel_cons 'low (__toplevel_cons 'high ()))
                        (lambda (_lhs0 _rhs0)
                           (if (eq? _lhs0 'low)
                              (if (eq? _rhs0 'low)
                                 'equal
                                 (if (eq? _rhs0 'high)
                                    'less
                                    (error "make-lattice base")))
                              (if (eq? _lhs0 'high)
                                 (if (eq? _rhs0 'low)
                                    'more
                                    (if (eq? _rhs0 'high)
                                       'equal
                                       (error "make-lattice base")))
                                 (error "make-lattice base"))))))
                (_l30 (maps _l20 _l20))
                (_l40 (maps _l30 _l30)))
            (count-maps _l20 _l20)
            (count-maps _l30 _l30)
            (count-maps _l20 _l30)
            (count-maps _l30 _l20)
            (count-maps _l40 _l40)))))
 
(= (run) 120549)
 
