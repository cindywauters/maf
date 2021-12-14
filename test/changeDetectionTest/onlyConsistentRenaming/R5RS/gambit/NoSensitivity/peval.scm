;; renamed lambdas/lets: 32
 
(define every? (<change>
      (lambda (pred? l)
         (let ((__or_res (null? l)))
            (if __or_res
               __or_res
               (if (pred? (car l)) (every? pred? (cdr l)) #f))))
      (lambda (_pred?0 _l0)
         (let ((___or_res0 (null? _l0)))
            (if ___or_res0
               ___or_res0
               (if (_pred?0 (car _l0))
                  (every? _pred?0 (cdr _l0))
                  #f))))))
 
(define some? (<change>
      (lambda (pred? l)
         (if (null? l)
            #f
            (let ((__or_res (pred? (car l))))
               (if __or_res __or_res (some? pred? (cdr l))))))
      (lambda (_pred?0 _l0)
         (if (null? _l0)
            #f
            (let ((___or_res0 (_pred?0 (car _l0))))
               (if ___or_res0
                  ___or_res0
                  (some? _pred?0 (cdr _l0))))))))
 
(define map2 (<change>
      (lambda (f l1 l2)
         (if (pair? l1)
            (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))
            ()))
      (lambda (_f0 _l10 _l20)
         (if (pair? _l10)
            (cons (_f0 (car _l10) (car _l20)) (map2 _f0 (cdr _l10) (cdr _l20)))
            ()))))
 
(define get-last-pair (<change>
      (lambda (l)
         (let ((x (cdr l)))
            (if (pair? x) (get-last-pair x) l)))
      (lambda (_l0)
         (let ((_x0 (cdr _l0)))
            (if (pair? _x0) (get-last-pair _x0) _l0)))))
 
(define partial-evaluate (<change>
      (lambda (proc args)
         (peval (alphatize proc ()) args))
      (lambda (_proc0 _args0)
         (peval (alphatize _proc0 ()) _args0))))
 
(define alphatize (<change>
      (lambda (exp env)
         (define alpha (lambda (exp)
               (if (const-expr? exp)
                  (quot (const-value exp))
                  (if (symbol? exp)
                     (let ((x (assq exp env)))
                        (if x (cdr x) exp))
                     (if (let ((__or_res (eq? (car exp) 'if))) (if __or_res __or_res (eq? (car exp) 'begin)))
                        (cons (car exp) (map alpha (cdr exp)))
                        (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                           (let ((new-env (new-variables (map car (cadr exp)) env)))
                              (list
                                 (car exp)
                                 (map
                                    (lambda (x)
                                       (list
                                          (cdr (assq (car x) new-env))
                                          (if (eq? (car exp) 'let)
                                             (alpha (cadr x))
                                             (alphatize (cadr x) new-env))))
                                    (cadr exp))
                                 (alphatize (caddr exp) new-env)))
                           (if (eq? (car exp) 'lambda)
                              (let ((new-env (new-variables (cadr exp) env)))
                                 (list 'lambda (map (lambda (x) (cdr (assq x new-env))) (cadr exp)) (alphatize (caddr exp) new-env)))
                              (map alpha exp))))))))
         (alpha exp))
      (lambda (_exp0 _env0)
         (define alpha (lambda (_exp1)
               (if (const-expr? _exp1)
                  (quot (const-value _exp1))
                  (if (symbol? _exp1)
                     (let ((_x0 (assq _exp1 _env0)))
                        (if _x0 (cdr _x0) _exp1))
                     (if (let ((___or_res0 (eq? (car _exp1) 'if))) (if ___or_res0 ___or_res0 (eq? (car _exp1) 'begin)))
                        (cons (car _exp1) (map alpha (cdr _exp1)))
                        (if (let ((___or_res1 (eq? (car _exp1) 'let))) (if ___or_res1 ___or_res1 (eq? (car _exp1) 'letrec)))
                           (let ((_new-env0 (new-variables (map car (cadr _exp1)) _env0)))
                              (list
                                 (car _exp1)
                                 (map
                                    (lambda (_x1)
                                       (list
                                          (cdr (assq (car _x1) _new-env0))
                                          (if (eq? (car _exp1) 'let)
                                             (alpha (cadr _x1))
                                             (alphatize (cadr _x1) _new-env0))))
                                    (cadr _exp1))
                                 (alphatize (caddr _exp1) _new-env0)))
                           (if (eq? (car _exp1) 'lambda)
                              (let ((_new-env1 (new-variables (cadr _exp1) _env0)))
                                 (list
                                    'lambda
                                    (map (lambda (_x2) (cdr (assq _x2 _new-env1))) (cadr _exp1))
                                    (alphatize (caddr _exp1) _new-env1)))
                              (map alpha _exp1))))))))
         (alpha _exp0))))
 
(define const-expr? (<change>
      (lambda (expr)
         (if (not (symbol? expr))
            (let ((__or_res (not (pair? expr))))
               (if __or_res __or_res (eq? (car expr) 'quote)))
            #f))
      (lambda (_expr0)
         (if (not (symbol? _expr0))
            (let ((___or_res0 (not (pair? _expr0))))
               (if ___or_res0
                  ___or_res0
                  (eq? (car _expr0) 'quote)))
            #f))))
 
(define const-value (<change>
      (lambda (expr)
         (if (pair? expr) (cadr expr) expr))
      (lambda (_expr0)
         (if (pair? _expr0) (cadr _expr0) _expr0))))
 
(define quot (<change>
      (lambda (val)
         (list 'quote val))
      (lambda (_val0)
         (list 'quote _val0))))
 
(define new-variables (<change>
      (lambda (parms env)
         (append (map (lambda (x) (cons x (new-variable x))) parms) env))
      (lambda (_parms0 _env0)
         (append (map (lambda (_x0) (cons _x0 (new-variable _x0))) _parms0) _env0))))
 
(define *current-num* 0)
 
(define new-variable (lambda (name)
      (set! *current-num* (+ *current-num* 1))
      (string->symbol (string-append (symbol->string name) "_" (number->string *current-num*)))))
 
(define peval (lambda (proc args)
      (simplify!
         (let ((parms (cadr proc))
               (body (caddr proc)))
            (list
               'lambda
               (remove-constant parms args)
               (beta-subst
                  body
                  (map2 (lambda (x y) (if (not-constant? y) (__toplevel_cons () ()) (cons x (quot y)))) parms args)))))))
 
(define not-constant (list '?))
 
(define not-constant? (<change>
      (lambda (x)
         (eq? x not-constant))
      (lambda (_x0)
         (eq? _x0 not-constant))))
 
(define remove-constant (<change>
      (lambda (l a)
         (if (null? l)
            ()
            (if (not-constant? (car a))
               (cons (car l) (remove-constant (cdr l) (cdr a)))
               (remove-constant (cdr l) (cdr a)))))
      (lambda (_l0 _a0)
         (if (null? _l0)
            ()
            (if (not-constant? (car _a0))
               (cons (car _l0) (remove-constant (cdr _l0) (cdr _a0)))
               (remove-constant (cdr _l0) (cdr _a0)))))))
 
(define extract-constant (lambda (l a)
      (if (null? l)
         ()
         (if (not-constant? (car a))
            (extract-constant (cdr l) (cdr a))
            (cons (car l) (extract-constant (cdr l) (cdr a)))))))
 
(define beta-subst (lambda (exp env)
      (define bs (lambda (exp)
            (if (const-expr? exp)
               (quot (const-value exp))
               (if (symbol? exp)
                  (let ((x (assq exp env)))
                     (if x (cdr x) exp))
                  (if (let ((__or_res (eq? (car exp) 'if))) (if __or_res __or_res (eq? (car exp) 'begin)))
                     (cons (car exp) (map bs (cdr exp)))
                     (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                        (list (car exp) (map (lambda (x) (list (car x) (bs (cadr x)))) (cadr exp)) (bs (caddr exp)))
                        (if (eq? (car exp) 'lambda)
                           (list 'lambda (cadr exp) (bs (caddr exp)))
                           (map bs exp))))))))
      (bs exp)))
 
(define simplify! (<change>
      (lambda (exp)
         (define simp! (lambda (where env)
               (define s! (lambda (where)
                     (let ((exp (car where)))
                        (let ((__cond-empty-body (const-expr? exp)))
                           (if __cond-empty-body
                              __cond-empty-body
                              (let ((__cond-empty-body (symbol? exp)))
                                 (if __cond-empty-body
                                    __cond-empty-body
                                    (if (eq? (car exp) 'if)
                                       (begin
                                          (s! (cdr exp))
                                          (if (const-expr? (cadr exp))
                                             (begin
                                                (set-car!
                                                   where
                                                   (if (memq (const-value (cadr exp)) (__toplevel_cons #f (__toplevel_cons () ())))
                                                      (if (= (length exp) 3)
                                                         (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                         (cadddr exp))
                                                      (caddr exp)))
                                                (s! where))
                                             (for-each! s! (cddr exp))))
                                       (if (eq? (car exp) 'begin)
                                          (begin
                                             (for-each! s! (cdr exp))
                                             ((letrec ((loop (lambda (exps)
                                                              (if (not (null? (cddr exps)))
                                                                 (let ((x (cadr exps)))
                                                                    (loop
                                                                       (if (let ((__or_res (const-expr? x))) (if __or_res __or_res (let ((__or_res (symbol? x))) (if __or_res __or_res (if (pair? x) (eq? (car x) 'lambda) #f)))))
                                                                          (begin
                                                                             (set-cdr! exps (cddr exps))
                                                                             exps)
                                                                          (cdr exps))))
                                                                 #f))))
                                                loop)
                                                exp)
                                             (if (null? (cddr exp))
                                                (set-car! where (cadr exp))
                                                #f))
                                          (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                             (let ((new-env (cons exp env)))
                                                (define keep (lambda (i)
                                                      (if (>= i (length (cadar where)))
                                                         ()
                                                         (let* ((var (car (list-ref (cadar where) i)))
                                                                (val (cadr (assq var (cadar where))))
                                                                (refs (ref-count (car where) var))
                                                                (self-refs (ref-count val var))
                                                                (total-refs (- (car refs) (car self-refs)))
                                                                (oper-refs (- (cadr refs) (cadr self-refs))))
                                                            (if (= total-refs 0)
                                                               (keep (+ i 1))
                                                               (if (let ((__or_res (const-expr? val))) (if __or_res __or_res (let ((__or_res (symbol? val))) (if __or_res __or_res (let ((__or_res (if (pair? val) (if (eq? (car val) 'lambda) (if (= total-refs 1) (if (= oper-refs 1) (= (car self-refs) 0) #f) #f) #f) #f))) (if __or_res __or_res (if (caddr refs) (= total-refs 1) #f)))))))
                                                                  (begin
                                                                     (set-car! where (beta-subst (car where) (list (cons var val))))
                                                                     (keep (+ i 1)))
                                                                  (cons var (keep (+ i 1)))))))))
                                                (simp! (cddr exp) new-env)
                                                (for-each! (lambda (x) (simp! (cdar x) new-env)) (cadr exp))
                                                (let ((to-keep (keep 0)))
                                                   (if (< (length to-keep) (length (cadar where)))
                                                      (begin
                                                         (if (null? to-keep)
                                                            (set-car! where (caddar where))
                                                            (set-car! (cdar where) (map (lambda (v) (assq v (cadar where))) to-keep)))
                                                         (s! where))
                                                      (if (null? to-keep)
                                                         (set-car! where (caddar where))
                                                         #f))))
                                             (if (eq? (car exp) 'lambda)
                                                (simp! (cddr exp) (cons exp env))
                                                (begin
                                                   (for-each! s! exp)
                                                   (if (symbol? (car exp))
                                                      (let ((frame (binding-frame (car exp) env)))
                                                         (if frame
                                                            (let ((proc (bound-expr (car exp) frame)))
                                                               (if (if (pair? proc) (if (eq? (car proc) 'lambda) (some? const-expr? (cdr exp)) #f) #f)
                                                                  (let* ((args (arg-pattern (cdr exp)))
                                                                         (new-proc (peval proc args))
                                                                         (new-args (remove-constant (cdr exp) args)))
                                                                     (set-car! where (cons (add-binding new-proc frame (car exp)) new-args)))
                                                                  #f))
                                                            (set-car! where (constant-fold-global (car exp) (cdr exp)))))
                                                      (let ((__cond-empty-body (not (pair? (car exp)))))
                                                         (if __cond-empty-body
                                                            __cond-empty-body
                                                            (if (eq? (caar exp) 'lambda)
                                                               (begin
                                                                  (set-car! where (list 'let (map2 list (cadar exp) (cdr exp)) (caddar exp)))
                                                                  (s! where))
                                                               #f))))))))))))))))
               (s! where)))
         (define remove-empty-calls! (lambda (where env)
               (define rec! (lambda (where)
                     (let ((exp (car where)))
                        (let ((__cond-empty-body (const-expr? exp)))
                           (if __cond-empty-body
                              __cond-empty-body
                              (let ((__cond-empty-body (symbol? exp)))
                                 (if __cond-empty-body
                                    __cond-empty-body
                                    (if (eq? (car exp) 'if)
                                       (begin
                                          (rec! (cdr exp))
                                          (rec! (cddr exp))
                                          (rec! (cdddr exp)))
                                       (if (eq? (car exp) 'begin)
                                          (for-each! rec! (cdr exp))
                                          (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                             (let ((new-env (cons exp env)))
                                                (remove-empty-calls! (cddr exp) new-env)
                                                (for-each! (lambda (x) (remove-empty-calls! (cdar x) new-env)) (cadr exp)))
                                             (if (eq? (car exp) 'lambda)
                                                (rec! (cddr exp))
                                                (begin
                                                   (for-each! rec! (cdr exp))
                                                   (if (if (null? (cdr exp)) (symbol? (car exp)) #f)
                                                      (let ((frame (binding-frame (car exp) env)))
                                                         (if frame
                                                            (let ((proc (bound-expr (car exp) frame)))
                                                               (if (if (pair? proc) (eq? (car proc) 'lambda) #f)
                                                                  (begin
                                                                     (set! changed? #t)
                                                                     (set-car! where (caddr proc)))
                                                                  #f))
                                                            #f))
                                                      #f)))))))))))))
               (rec! where)))
         (define changed? #f)
         (let ((x (list exp)))
            ((letrec ((loop (lambda ()
                             (set! changed? #f)
                             (simp! x ())
                             (remove-empty-calls! x ())
                             (if changed? (loop) (car x)))))
               loop))))
      (lambda (_exp0)
         (define simp! (lambda (_where0 _env0)
               (define s! (lambda (_where1)
                     (let ((_exp1 (car _where1)))
                        (let ((___cond-empty-body0 (const-expr? _exp1)))
                           (if ___cond-empty-body0
                              ___cond-empty-body0
                              (let ((___cond-empty-body1 (symbol? _exp1)))
                                 (if ___cond-empty-body1
                                    ___cond-empty-body1
                                    (if (eq? (car _exp1) 'if)
                                       (begin
                                          (s! (cdr _exp1))
                                          (if (const-expr? (cadr _exp1))
                                             (begin
                                                (set-car!
                                                   _where1
                                                   (if (memq (const-value (cadr _exp1)) (__toplevel_cons #f (__toplevel_cons () ())))
                                                      (if (= (length _exp1) 3)
                                                         (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                         (cadddr _exp1))
                                                      (caddr _exp1)))
                                                (s! _where1))
                                             (for-each! s! (cddr _exp1))))
                                       (if (eq? (car _exp1) 'begin)
                                          (begin
                                             (for-each! s! (cdr _exp1))
                                             ((letrec ((_loop0 (lambda (_exps0)
                                                                (if (not (null? (cddr _exps0)))
                                                                   (let ((_x0 (cadr _exps0)))
                                                                      (_loop0
                                                                         (if (let ((___or_res0 (const-expr? _x0))) (if ___or_res0 ___or_res0 (let ((___or_res1 (symbol? _x0))) (if ___or_res1 ___or_res1 (if (pair? _x0) (eq? (car _x0) 'lambda) #f)))))
                                                                            (begin
                                                                               (set-cdr! _exps0 (cddr _exps0))
                                                                               _exps0)
                                                                            (cdr _exps0))))
                                                                   #f))))
                                                _loop0)
                                                _exp1)
                                             (if (null? (cddr _exp1))
                                                (set-car! _where1 (cadr _exp1))
                                                #f))
                                          (if (let ((___or_res2 (eq? (car _exp1) 'let))) (if ___or_res2 ___or_res2 (eq? (car _exp1) 'letrec)))
                                             (let ((_new-env0 (cons _exp1 _env0)))
                                                (define keep (lambda (_i0)
                                                      (if (>= _i0 (length (cadar _where1)))
                                                         ()
                                                         (let* ((_var0 (car (list-ref (cadar _where1) _i0)))
                                                                (_val0 (cadr (assq _var0 (cadar _where1))))
                                                                (_refs0 (ref-count (car _where1) _var0))
                                                                (_self-refs0 (ref-count _val0 _var0))
                                                                (_total-refs0 (- (car _refs0) (car _self-refs0)))
                                                                (_oper-refs0 (- (cadr _refs0) (cadr _self-refs0))))
                                                            (if (= _total-refs0 0)
                                                               (keep (+ _i0 1))
                                                               (if (let ((___or_res3 (const-expr? _val0))) (if ___or_res3 ___or_res3 (let ((___or_res4 (symbol? _val0))) (if ___or_res4 ___or_res4 (let ((___or_res5 (if (pair? _val0) (if (eq? (car _val0) 'lambda) (if (= _total-refs0 1) (if (= _oper-refs0 1) (= (car _self-refs0) 0) #f) #f) #f) #f))) (if ___or_res5 ___or_res5 (if (caddr _refs0) (= _total-refs0 1) #f)))))))
                                                                  (begin
                                                                     (set-car! _where1 (beta-subst (car _where1) (list (cons _var0 _val0))))
                                                                     (keep (+ _i0 1)))
                                                                  (cons _var0 (keep (+ _i0 1)))))))))
                                                (simp! (cddr _exp1) _new-env0)
                                                (for-each! (lambda (_x1) (simp! (cdar _x1) _new-env0)) (cadr _exp1))
                                                (let ((_to-keep0 (keep 0)))
                                                   (if (< (length _to-keep0) (length (cadar _where1)))
                                                      (begin
                                                         (if (null? _to-keep0)
                                                            (set-car! _where1 (caddar _where1))
                                                            (set-car! (cdar _where1) (map (lambda (_v0) (assq _v0 (cadar _where1))) _to-keep0)))
                                                         (s! _where1))
                                                      (if (null? _to-keep0)
                                                         (set-car! _where1 (caddar _where1))
                                                         #f))))
                                             (if (eq? (car _exp1) 'lambda)
                                                (simp! (cddr _exp1) (cons _exp1 _env0))
                                                (begin
                                                   (for-each! s! _exp1)
                                                   (if (symbol? (car _exp1))
                                                      (let ((_frame0 (binding-frame (car _exp1) _env0)))
                                                         (if _frame0
                                                            (let ((_proc0 (bound-expr (car _exp1) _frame0)))
                                                               (if (if (pair? _proc0) (if (eq? (car _proc0) 'lambda) (some? const-expr? (cdr _exp1)) #f) #f)
                                                                  (let* ((_args0 (arg-pattern (cdr _exp1)))
                                                                         (_new-proc0 (peval _proc0 _args0))
                                                                         (_new-args0 (remove-constant (cdr _exp1) _args0)))
                                                                     (set-car! _where1 (cons (add-binding _new-proc0 _frame0 (car _exp1)) _new-args0)))
                                                                  #f))
                                                            (set-car! _where1 (constant-fold-global (car _exp1) (cdr _exp1)))))
                                                      (let ((___cond-empty-body2 (not (pair? (car _exp1)))))
                                                         (if ___cond-empty-body2
                                                            ___cond-empty-body2
                                                            (if (eq? (caar _exp1) 'lambda)
                                                               (begin
                                                                  (set-car! _where1 (list 'let (map2 list (cadar _exp1) (cdr _exp1)) (caddar _exp1)))
                                                                  (s! _where1))
                                                               #f))))))))))))))))
               (s! _where0)))
         (define remove-empty-calls! (lambda (_where2 _env1)
               (define rec! (lambda (_where3)
                     (let ((_exp2 (car _where3)))
                        (let ((___cond-empty-body3 (const-expr? _exp2)))
                           (if ___cond-empty-body3
                              ___cond-empty-body3
                              (let ((___cond-empty-body4 (symbol? _exp2)))
                                 (if ___cond-empty-body4
                                    ___cond-empty-body4
                                    (if (eq? (car _exp2) 'if)
                                       (begin
                                          (rec! (cdr _exp2))
                                          (rec! (cddr _exp2))
                                          (rec! (cdddr _exp2)))
                                       (if (eq? (car _exp2) 'begin)
                                          (for-each! rec! (cdr _exp2))
                                          (if (let ((___or_res6 (eq? (car _exp2) 'let))) (if ___or_res6 ___or_res6 (eq? (car _exp2) 'letrec)))
                                             (let ((_new-env1 (cons _exp2 _env1)))
                                                (remove-empty-calls! (cddr _exp2) _new-env1)
                                                (for-each! (lambda (_x2) (remove-empty-calls! (cdar _x2) _new-env1)) (cadr _exp2)))
                                             (if (eq? (car _exp2) 'lambda)
                                                (rec! (cddr _exp2))
                                                (begin
                                                   (for-each! rec! (cdr _exp2))
                                                   (if (if (null? (cdr _exp2)) (symbol? (car _exp2)) #f)
                                                      (let ((_frame1 (binding-frame (car _exp2) _env1)))
                                                         (if _frame1
                                                            (let ((_proc1 (bound-expr (car _exp2) _frame1)))
                                                               (if (if (pair? _proc1) (eq? (car _proc1) 'lambda) #f)
                                                                  (begin
                                                                     (set! changed? #t)
                                                                     (set-car! _where3 (caddr _proc1)))
                                                                  #f))
                                                            #f))
                                                      #f)))))))))))))
               (rec! _where2)))
         (define changed? #f)
         (let ((_x3 (list _exp0)))
            ((letrec ((_loop1 (lambda ()
                               (set! changed? #f)
                               (simp! _x3 ())
                               (remove-empty-calls! _x3 ())
                               (if changed? (_loop1) (car _x3)))))
               _loop1))))))
 
(define ref-count (lambda (exp var)
      (<change>
         (let ((total 0)
               (oper 0)
               (always-evaled #t))
            (define rc (lambda (exp ae)
                  (let ((__cond-empty-body (const-expr? exp)))
                     (if __cond-empty-body
                        __cond-empty-body
                        (if (symbol? exp)
                           (if (eq? exp var)
                              (begin
                                 (set! total (+ total 1))
                                 (set! always-evaled (if ae always-evaled #f)))
                              #f)
                           (if (eq? (car exp) 'if)
                              (begin
                                 (rc (cadr exp) ae)
                                 (for-each (lambda (x) (rc x #f)) (cddr exp)))
                              (if (eq? (car exp) 'begin)
                                 (for-each (lambda (x) (rc x ae)) (cdr exp))
                                 (if (let ((__or_res (eq? (car exp) 'let))) (if __or_res __or_res (eq? (car exp) 'letrec)))
                                    (begin
                                       (for-each (lambda (x) (rc (cadr x) ae)) (cadr exp))
                                       (rc (caddr exp) ae))
                                    (if (eq? (car exp) 'lambda)
                                       (rc (caddr exp) #f)
                                       (begin
                                          (for-each (lambda (x) (rc x ae)) exp)
                                          (if (symbol? (car exp))
                                             (if (eq? (car exp) var)
                                                (set! oper (+ oper 1))
                                                #f)
                                             #f)))))))))))
            (rc exp #t)
            (list total oper always-evaled))
         (let ((_total0 0)
               (_oper0 0)
               (_always-evaled0 #t))
            (define rc (lambda (_exp0 _ae0)
                  (let ((___cond-empty-body0 (const-expr? _exp0)))
                     (if ___cond-empty-body0
                        ___cond-empty-body0
                        (if (symbol? _exp0)
                           (if (eq? _exp0 var)
                              (begin
                                 (set! _total0 (+ _total0 1))
                                 (set! _always-evaled0 (if _ae0 _always-evaled0 #f)))
                              #f)
                           (if (eq? (car _exp0) 'if)
                              (begin
                                 (rc (cadr _exp0) _ae0)
                                 (for-each (lambda (_x0) (rc _x0 #f)) (cddr _exp0)))
                              (if (eq? (car _exp0) 'begin)
                                 (for-each (lambda (_x1) (rc _x1 _ae0)) (cdr _exp0))
                                 (if (let ((___or_res0 (eq? (car _exp0) 'let))) (if ___or_res0 ___or_res0 (eq? (car _exp0) 'letrec)))
                                    (begin
                                       (for-each (lambda (_x2) (rc (cadr _x2) _ae0)) (cadr _exp0))
                                       (rc (caddr _exp0) _ae0))
                                    (if (eq? (car _exp0) 'lambda)
                                       (rc (caddr _exp0) #f)
                                       (begin
                                          (for-each (lambda (_x3) (rc _x3 _ae0)) _exp0)
                                          (if (symbol? (car _exp0))
                                             (if (eq? (car _exp0) var)
                                                (set! _oper0 (+ _oper0 1))
                                                #f)
                                             #f)))))))))))
            (rc exp #t)
            (list _total0 _oper0 _always-evaled0)))))
 
(define binding-frame (<change>
      (lambda (var env)
         (if (null? env)
            #f
            (if (let ((__or_res (eq? (caar env) 'let))) (if __or_res __or_res (eq? (caar env) 'letrec)))
               (if (assq var (cadar env))
                  (car env)
                  (binding-frame var (cdr env)))
               (if (eq? (caar env) 'lambda)
                  (if (memq var (cadar env))
                     (car env)
                     (binding-frame var (cdr env)))
                  (error "ill-formed environment")))))
      (lambda (_var0 _env0)
         (if (null? _env0)
            #f
            (if (let ((___or_res0 (eq? (caar _env0) 'let))) (if ___or_res0 ___or_res0 (eq? (caar _env0) 'letrec)))
               (if (assq _var0 (cadar _env0))
                  (car _env0)
                  (binding-frame _var0 (cdr _env0)))
               (if (eq? (caar _env0) 'lambda)
                  (if (memq _var0 (cadar _env0))
                     (car _env0)
                     (binding-frame _var0 (cdr _env0)))
                  (error "ill-formed environment")))))))
 
(define bound-expr (<change>
      (lambda (var frame)
         (if (let ((__or_res (eq? (car frame) 'let))) (if __or_res __or_res (eq? (car frame) 'letrec)))
            (cadr (assq var (cadr frame)))
            (if (eq? (car frame) 'lambda)
               not-constant
               (error "ill-formed frame"))))
      (lambda (_var0 _frame0)
         (if (let ((___or_res0 (eq? (car _frame0) 'let))) (if ___or_res0 ___or_res0 (eq? (car _frame0) 'letrec)))
            (cadr (assq _var0 (cadr _frame0)))
            (if (eq? (car _frame0) 'lambda)
               not-constant
               (error "ill-formed frame"))))))
 
(define add-binding (<change>
      (lambda (val frame name)
         (define find-val (lambda (val bindings)
               (if (null? bindings)
                  #f
                  (if (equal? val (cadar bindings))
                     (caar bindings)
                     (find-val val (cdr bindings))))))
         (let ((__or_res (find-val val (cadr frame))))
            (if __or_res
               __or_res
               (let ((var (new-variable name)))
                  (set-cdr! (get-last-pair (cadr frame)) (list (list var val)))
                  var))))
      (lambda (_val0 _frame0 _name0)
         (define find-val (lambda (_val1 _bindings0)
               (if (null? _bindings0)
                  #f
                  (if (equal? _val1 (cadar _bindings0))
                     (caar _bindings0)
                     (find-val _val1 (cdr _bindings0))))))
         (let ((___or_res0 (find-val _val0 (cadr _frame0))))
            (if ___or_res0
               ___or_res0
               (let ((_var0 (new-variable _name0)))
                  (set-cdr! (get-last-pair (cadr _frame0)) (list (list _var0 _val0)))
                  _var0))))))
 
(define for-each! (lambda (proc! l)
      (if (not (null? l))
         (begin
            (proc! l)
            (for-each! proc! (cdr l)))
         #f)))
 
(define arg-pattern (<change>
      (lambda (exps)
         (if (null? exps)
            ()
            (cons (if (const-expr? (car exps)) (const-value (car exps)) not-constant) (arg-pattern (cdr exps)))))
      (lambda (_exps0)
         (if (null? _exps0)
            ()
            (cons
               (if (const-expr? (car _exps0))
                  (const-value (car _exps0))
                  not-constant)
               (arg-pattern (cdr _exps0)))))))
 
(define *primitives* (list
      (cons
         'car
         (lambda (args)
            (if (= (length args) 1)
               (if (pair? (car args))
                  (quot (car (car args)))
                  #f)
               #f)))
      (cons
         'cdr
         (lambda (args)
            (if (= (length args) 1)
               (if (pair? (car args))
                  (quot (cdr (car args)))
                  #f)
               #f)))
      (cons '+ (lambda (args) (if (every? number? args) (quot (sum args 0)) #f)))
      (cons '* (lambda (args) (if (every? number? args) (quot (product args 1)) #f)))
      (cons
         '-
         (lambda (args)
            (if (> (length args) 0)
               (if (every? number? args)
                  (quot (if (null? (cdr args)) (- (car args)) (- (car args) (sum (cdr args) 0))))
                  #f)
               #f)))
      (cons
         '/
         (lambda (args)
            (if (> (length args) 1)
               (if (every? number? args)
                  (quot (if (null? (cdr args)) (/ (car args)) (/ (car args) (product (cdr args) 1))))
                  #f)
               #f)))
      (cons
         '<
         (lambda (args)
            (if (= (length args) 2)
               (if (every? number? args)
                  (quot (< (car args) (cadr args)))
                  #f)
               #f)))
      (cons
         '=
         (lambda (args)
            (if (= (length args) 2)
               (if (every? number? args)
                  (quot (= (car args) (cadr args)))
                  #f)
               #f)))
      (cons
         '>
         (lambda (args)
            (if (= (length args) 2)
               (if (every? number? args)
                  (quot (> (car args) (cadr args)))
                  #f)
               #f)))
      (cons 'eq? (lambda (args) (if (= (length args) 2) (quot (eq? (car args) (cadr args))) #f)))
      (cons 'not (lambda (args) (if (= (length args) 1) (quot (not (car args))) #f)))
      (cons 'null? (lambda (args) (if (= (length args) 1) (quot (null? (car args))) #f)))
      (cons 'pair? (lambda (args) (if (= (length args) 1) (quot (pair? (car args))) #f)))
      (cons 'symbol? (lambda (args) (if (= (length args) 1) (quot (symbol? (car args))) #f)))))
 
(define sum (lambda (lst n)
      (if (null? lst)
         n
         (sum (cdr lst) (+ n (car lst))))))
 
(define product (lambda (lst n)
      (if (null? lst)
         n
         (product (cdr lst) (* n (car lst))))))
 
(define reduce-global (<change>
      (lambda (name args)
         (let ((x (assq name *primitives*)))
            (if x ((cdr x) args) #f)))
      (lambda (_name0 _args0)
         (let ((_x0 (assq _name0 *primitives*)))
            (if _x0 ((cdr _x0) _args0) #f)))))
 
(define constant-fold-global (lambda (name exprs)
      (define flatten (<change>
            (lambda (args op)
               (if (null? args)
                  ()
                  (if (if (pair? (car args)) (eq? (caar args) op) #f)
                     (append (flatten (cdar args) op) (flatten (cdr args) op))
                     (cons (car args) (flatten (cdr args) op)))))
            (lambda (_args0 _op0)
               (if (null? _args0)
                  ()
                  (if (if (pair? (car _args0)) (eq? (caar _args0) _op0) #f)
                     (append (flatten (cdar _args0) _op0) (flatten (cdr _args0) _op0))
                     (cons (car _args0) (flatten (cdr _args0) _op0)))))))
      (<change>
         (let ((args (if (let ((__or_res (eq? name '+))) (if __or_res __or_res (eq? name '*)))
                       (flatten exprs name)
                       exprs)))
            (let ((__or_res (if (every? const-expr? args)
                              (reduce-global name (map const-value args))
                              #f)))
               (if __or_res
                  __or_res
                  (let ((pattern (arg-pattern args)))
                     (let ((non-const (remove-constant args pattern))
                           (const (map const-value (extract-constant args pattern))))
                        (if (eq? name '+)
                           (let ((x (reduce-global '+ const)))
                              (if x
                                 (let ((y (const-value x)))
                                    (cons '+ (if (= y 0) non-const (cons x non-const))))
                                 (cons name args)))
                           (if (eq? name '*)
                              (let ((x (reduce-global '* const)))
                                 (if x
                                    (let ((y (const-value x)))
                                       (cons '* (if (= y 1) non-const (cons x non-const))))
                                    (cons name args)))
                              (if (eq? name 'cons)
                                 (if (if (const-expr? (cadr args)) (null? (const-value (cadr args))) #f)
                                    (list 'list (car args))
                                    (if (if (pair? (cadr args)) (eq? (car (cadr args)) 'list) #f)
                                       (cons 'list (cons (car args) (cdr (cadr args))))
                                       (cons name args)))
                                 (cons name args)))))))))
         (let ((_args0 (if (let ((___or_res0 (eq? name '+))) (if ___or_res0 ___or_res0 (eq? name '*)))
                         (flatten exprs name)
                         exprs)))
            (let ((___or_res1 (if (every? const-expr? _args0)
                                (reduce-global name (map const-value _args0))
                                #f)))
               (if ___or_res1
                  ___or_res1
                  (let ((_pattern0 (arg-pattern _args0)))
                     (let ((_non-const0 (remove-constant _args0 _pattern0))
                           (_const0 (map const-value (extract-constant _args0 _pattern0))))
                        (if (eq? name '+)
                           (let ((_x0 (reduce-global '+ _const0)))
                              (if _x0
                                 (let ((_y0 (const-value _x0)))
                                    (cons '+ (if (= _y0 0) _non-const0 (cons _x0 _non-const0))))
                                 (cons name _args0)))
                           (if (eq? name '*)
                              (let ((_x1 (reduce-global '* _const0)))
                                 (if _x1
                                    (let ((_y1 (const-value _x1)))
                                       (cons '* (if (= _y1 1) _non-const0 (cons _x1 _non-const0))))
                                    (cons name _args0)))
                              (if (eq? name 'cons)
                                 (if (if (const-expr? (cadr _args0)) (null? (const-value (cadr _args0))) #f)
                                    (list 'list (car _args0))
                                    (if (if (pair? (cadr _args0)) (eq? (car (cadr _args0)) 'list) #f)
                                       (cons 'list (cons (car _args0) (cdr (cadr _args0))))
                                       (cons name _args0)))
                                 (cons name _args0))))))))))))
 
(define try-peval (<change>
      (lambda (proc args)
         (partial-evaluate proc args))
      (lambda (_proc0 _args0)
         (partial-evaluate _proc0 _args0))))
 
(define example1 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ())))
         (__toplevel_cons
            (__toplevel_cons
               'if
               (__toplevel_cons
                  (__toplevel_cons 'null? (__toplevel_cons 'a ()))
                  (__toplevel_cons
                     'b
                     (__toplevel_cons
                        (__toplevel_cons
                           '+
                           (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'a ())) (__toplevel_cons 'c ())))
                        ()))))
            ()))))
 
(define example2 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'x (__toplevel_cons 'y ()))
         (__toplevel_cons
            (__toplevel_cons
               'let
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'q
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'a (__toplevel_cons 'b ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons '< (__toplevel_cons 'a (__toplevel_cons 0 ())))
                                          (__toplevel_cons
                                             'b
                                             (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 10 (__toplevel_cons 'b ()))) ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'if
                        (__toplevel_cons
                           (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 0 ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'q
                                 (__toplevel_cons
                                    (__toplevel_cons '- (__toplevel_cons 'y ()))
                                    (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x ())) ())))
                              (__toplevel_cons (__toplevel_cons 'q (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))))
                     ())))
            ()))))
 
(define example3 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'l (__toplevel_cons 'n ()))
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'add-list
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'l (__toplevel_cons 'n ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons 'null? (__toplevel_cons 'l ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'quote (__toplevel_cons () ()))
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'cons
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         '+
                                                         (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'l ())) (__toplevel_cons 'n ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'add-list
                                                            (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'l ())) (__toplevel_cons 'n ())))
                                                         ())))
                                                ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons (__toplevel_cons 'add-list (__toplevel_cons 'l (__toplevel_cons 'n ()))) ())))
            ()))))
 
(define example4 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'exp (__toplevel_cons 'env ()))
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'eval
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'exp (__toplevel_cons 'env ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'letrec
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'eval-list
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'lambda
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'l (__toplevel_cons 'env ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'if
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'null? (__toplevel_cons 'l ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'quote (__toplevel_cons () ()))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'cons
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons
                                                                                 'eval
                                                                                 (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'l ())) (__toplevel_cons 'env ())))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'eval-list
                                                                                    (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'l ())) (__toplevel_cons 'env ())))
                                                                                 ())))
                                                                        ()))))
                                                            ())))
                                                   ()))
                                             ())
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'if
                                                (__toplevel_cons
                                                   (__toplevel_cons 'symbol? (__toplevel_cons 'exp ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'lookup (__toplevel_cons 'exp (__toplevel_cons 'env ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'if
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'pair? (__toplevel_cons 'exp ())) ()))
                                                               (__toplevel_cons
                                                                  'exp
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'if
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'eq?
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'car (__toplevel_cons 'exp ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons 'quote ())) ())))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'car (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'exp ())) ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'apply
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'eval
                                                                                          (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'exp ())) (__toplevel_cons 'env ())))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons
                                                                                             'eval-list
                                                                                             (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'exp ())) (__toplevel_cons 'env ())))
                                                                                          ())))
                                                                                 ()))))
                                                                     ()))))
                                                         ()))))
                                             ())))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons (__toplevel_cons 'eval (__toplevel_cons 'exp (__toplevel_cons 'env ()))) ())))
            ()))))
 
(define example5 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 'b ()))
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'funct
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'x ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       '+
                                       (__toplevel_cons
                                          'x
                                          (__toplevel_cons
                                             'b
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'if
                                                   (__toplevel_cons
                                                      (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 1 ())))
                                                      (__toplevel_cons
                                                         0
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'funct
                                                               (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 1 ()))) ()))
                                                            ()))))
                                                ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons (__toplevel_cons 'funct (__toplevel_cons 'a ())) ())))
            ()))))
 
(define example6 (__toplevel_cons
      'lambda
      (__toplevel_cons
         ()
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'fib
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'x ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons '< (__toplevel_cons 'x (__toplevel_cons 2 ())))
                                          (__toplevel_cons
                                             'x
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   '+
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'fib
                                                         (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 1 ()))) ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'fib
                                                            (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'x (__toplevel_cons 2 ()))) ()))
                                                         ())))
                                                ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons (__toplevel_cons 'fib (__toplevel_cons 10 ())) ())))
            ()))))
 
(define example7 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'input ())
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'copy
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'in ())
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons 'pair? (__toplevel_cons 'in ()))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'cons
                                                (__toplevel_cons
                                                   (__toplevel_cons 'copy (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'in ())) ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'copy (__toplevel_cons (__toplevel_cons 'cdr (__toplevel_cons 'in ())) ()))
                                                      ())))
                                             (__toplevel_cons 'in ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons (__toplevel_cons 'copy (__toplevel_cons 'input ())) ())))
            ()))))
 
(define example8 (__toplevel_cons
      'lambda
      (__toplevel_cons
         (__toplevel_cons 'input ())
         (__toplevel_cons
            (__toplevel_cons
               'letrec
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        'reverse
                        (__toplevel_cons
                           (__toplevel_cons
                              'lambda
                              (__toplevel_cons
                                 (__toplevel_cons 'in (__toplevel_cons 'result ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'if
                                       (__toplevel_cons
                                          (__toplevel_cons 'pair? (__toplevel_cons 'in ()))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'reverse
                                                (__toplevel_cons
                                                   (__toplevel_cons 'cdr (__toplevel_cons 'in ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'cons
                                                         (__toplevel_cons (__toplevel_cons 'car (__toplevel_cons 'in ())) (__toplevel_cons 'result ())))
                                                      ())))
                                             (__toplevel_cons 'result ()))))
                                    ())))
                           ()))
                     ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'reverse
                        (__toplevel_cons 'input (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons () ())) ())))
                     ())))
            ()))))
 
(define test (<change>
      (lambda ()
         (set! *current-num* 0)
         (list
            (try-peval example1 (list (__toplevel_cons 10 (__toplevel_cons 11 ())) not-constant 1))
            (try-peval example2 (list not-constant 1))
            (try-peval example3 (list not-constant 1))
            (try-peval
               example3
               (list (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) not-constant))
            (try-peval example4 (list 'x not-constant))
            (try-peval
               example4
               (list
                  (__toplevel_cons 'f (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
                  not-constant))
            (try-peval example5 (list 5 not-constant))
            (try-peval example6 ())
            (try-peval
               example7
               (list
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'c
                           (__toplevel_cons
                              'd
                              (__toplevel_cons
                                 'e
                                 (__toplevel_cons
                                    'f
                                    (__toplevel_cons
                                       'g
                                       (__toplevel_cons
                                          'h
                                          (__toplevel_cons
                                             'i
                                             (__toplevel_cons
                                                'j
                                                (__toplevel_cons
                                                   'k
                                                   (__toplevel_cons
                                                      'l
                                                      (__toplevel_cons
                                                         'm
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               'o
                                                               (__toplevel_cons
                                                                  'p
                                                                  (__toplevel_cons
                                                                     'q
                                                                     (__toplevel_cons
                                                                        'r
                                                                        (__toplevel_cons
                                                                           's
                                                                           (__toplevel_cons
                                                                              't
                                                                              (__toplevel_cons
                                                                                 'u
                                                                                 (__toplevel_cons
                                                                                    'v
                                                                                    (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))
            (try-peval
               example8
               (list
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'c
                           (__toplevel_cons
                              'd
                              (__toplevel_cons
                                 'e
                                 (__toplevel_cons
                                    'f
                                    (__toplevel_cons
                                       'g
                                       (__toplevel_cons
                                          'h
                                          (__toplevel_cons
                                             'i
                                             (__toplevel_cons
                                                'j
                                                (__toplevel_cons
                                                   'k
                                                   (__toplevel_cons
                                                      'l
                                                      (__toplevel_cons
                                                         'm
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               'o
                                                               (__toplevel_cons
                                                                  'p
                                                                  (__toplevel_cons
                                                                     'q
                                                                     (__toplevel_cons
                                                                        'r
                                                                        (__toplevel_cons
                                                                           's
                                                                           (__toplevel_cons
                                                                              't
                                                                              (__toplevel_cons
                                                                                 'u
                                                                                 (__toplevel_cons
                                                                                    'v
                                                                                    (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))))
      (lambda ()
         (set! *current-num* 0)
         (list
            (try-peval example1 (list (__toplevel_cons 10 (__toplevel_cons 11 ())) not-constant 1))
            (try-peval example2 (list not-constant 1))
            (try-peval example3 (list not-constant 1))
            (try-peval
               example3
               (list (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) not-constant))
            (try-peval example4 (list 'x not-constant))
            (try-peval
               example4
               (list
                  (__toplevel_cons 'f (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
                  not-constant))
            (try-peval example5 (list 5 not-constant))
            (try-peval example6 ())
            (try-peval
               example7
               (list
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'c
                           (__toplevel_cons
                              'd
                              (__toplevel_cons
                                 'e
                                 (__toplevel_cons
                                    'f
                                    (__toplevel_cons
                                       'g
                                       (__toplevel_cons
                                          'h
                                          (__toplevel_cons
                                             'i
                                             (__toplevel_cons
                                                'j
                                                (__toplevel_cons
                                                   'k
                                                   (__toplevel_cons
                                                      'l
                                                      (__toplevel_cons
                                                         'm
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               'o
                                                               (__toplevel_cons
                                                                  'p
                                                                  (__toplevel_cons
                                                                     'q
                                                                     (__toplevel_cons
                                                                        'r
                                                                        (__toplevel_cons
                                                                           's
                                                                           (__toplevel_cons
                                                                              't
                                                                              (__toplevel_cons
                                                                                 'u
                                                                                 (__toplevel_cons
                                                                                    'v
                                                                                    (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))
            (try-peval
               example8
               (list
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'c
                           (__toplevel_cons
                              'd
                              (__toplevel_cons
                                 'e
                                 (__toplevel_cons
                                    'f
                                    (__toplevel_cons
                                       'g
                                       (__toplevel_cons
                                          'h
                                          (__toplevel_cons
                                             'i
                                             (__toplevel_cons
                                                'j
                                                (__toplevel_cons
                                                   'k
                                                   (__toplevel_cons
                                                      'l
                                                      (__toplevel_cons
                                                         'm
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               'o
                                                               (__toplevel_cons
                                                                  'p
                                                                  (__toplevel_cons
                                                                     'q
                                                                     (__toplevel_cons
                                                                        'r
                                                                        (__toplevel_cons
                                                                           's
                                                                           (__toplevel_cons
                                                                              't
                                                                              (__toplevel_cons
                                                                                 'u
                                                                                 (__toplevel_cons
                                                                                    'v
                                                                                    (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))))))))))))))))))))))))))))
 
(<change>
   (let ((result (test)))
      (if (list? result)
         (if (= (length result) 10)
            (equal?
               (list-ref result 9)
               (__toplevel_cons
                  'lambda
                  (__toplevel_cons
                     ()
                     (__toplevel_cons
                        (__toplevel_cons
                           'list
                           (__toplevel_cons
                              (__toplevel_cons 'quote (__toplevel_cons 'z ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'quote (__toplevel_cons 'y ()))
                                 (__toplevel_cons
                                    (__toplevel_cons 'quote (__toplevel_cons 'x ()))
                                    (__toplevel_cons
                                       (__toplevel_cons 'quote (__toplevel_cons 'w ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'quote (__toplevel_cons 'v ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'quote (__toplevel_cons 'u ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'quote (__toplevel_cons 't ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons 'quote (__toplevel_cons 's ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'quote (__toplevel_cons 'r ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'quote (__toplevel_cons 'q ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'quote (__toplevel_cons 'p ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'quote (__toplevel_cons 'o ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'quote (__toplevel_cons 'n ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'quote (__toplevel_cons 'm ()))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'quote (__toplevel_cons 'l ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons 'quote (__toplevel_cons 'k ()))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'quote (__toplevel_cons 'j ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'quote (__toplevel_cons 'i ()))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'quote (__toplevel_cons 'h ()))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons 'quote (__toplevel_cons 'g ()))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'quote (__toplevel_cons 'f ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'quote (__toplevel_cons 'e ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons 'quote (__toplevel_cons 'd ()))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'quote (__toplevel_cons 'c ()))
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons 'quote (__toplevel_cons 'b ()))
                                                                                                      (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons 'a ())) ())))))))))))))))))))))))))))
                        ()))))
            #f)
         #f))
   (let ((_result0 (test)))
      (if (list? _result0)
         (if (= (length _result0) 10)
            (equal?
               (list-ref _result0 9)
               (__toplevel_cons
                  'lambda
                  (__toplevel_cons
                     ()
                     (__toplevel_cons
                        (__toplevel_cons
                           'list
                           (__toplevel_cons
                              (__toplevel_cons 'quote (__toplevel_cons 'z ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'quote (__toplevel_cons 'y ()))
                                 (__toplevel_cons
                                    (__toplevel_cons 'quote (__toplevel_cons 'x ()))
                                    (__toplevel_cons
                                       (__toplevel_cons 'quote (__toplevel_cons 'w ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'quote (__toplevel_cons 'v ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'quote (__toplevel_cons 'u ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'quote (__toplevel_cons 't ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons 'quote (__toplevel_cons 's ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'quote (__toplevel_cons 'r ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'quote (__toplevel_cons 'q ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'quote (__toplevel_cons 'p ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'quote (__toplevel_cons 'o ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'quote (__toplevel_cons 'n ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'quote (__toplevel_cons 'm ()))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'quote (__toplevel_cons 'l ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons 'quote (__toplevel_cons 'k ()))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'quote (__toplevel_cons 'j ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'quote (__toplevel_cons 'i ()))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'quote (__toplevel_cons 'h ()))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons 'quote (__toplevel_cons 'g ()))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'quote (__toplevel_cons 'f ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'quote (__toplevel_cons 'e ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons 'quote (__toplevel_cons 'd ()))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'quote (__toplevel_cons 'c ()))
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons 'quote (__toplevel_cons 'b ()))
                                                                                                      (__toplevel_cons (__toplevel_cons 'quote (__toplevel_cons 'a ())) ())))))))))))))))))))))))))))
                        ()))))
            #f)
         #f)))
 
