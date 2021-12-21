;; renamed lambdas/lets: 64
 
(define void (<change>
      (lambda ()
         (if #f #t #f))
      (lambda ()
         (if #f #t #f))))
 
(define tagged-list? (lambda (tag l)
      (if (pair? l) (eq? tag (car l)) #f)))
 
(define char->natural (lambda (c)
      (<change>
         (let ((i (char->integer c)))
            (if (< i 0) (* -2 i) (+ (* 2 i) 1)))
         (let ((_i0 (char->integer c)))
            (if (< _i0 0) (* -2 _i0) (+ (* 2 _i0) 1))))))
 
(define integer->char-list (<change>
      (lambda (n)
         (string->list (number->string n)))
      (lambda (_n0)
         (string->list (number->string _n0)))))
 
(define gensym-count 0)
 
(define gensym (lambda (param)
      (set! gensym-count (+ gensym-count 1))
      (string->symbol
         (string-append (if (symbol? param) (symbol->string param) param) "$" (number->string gensym-count)))))
 
(define symbol<? (<change>
      (lambda (sym1 sym2)
         (string<? (symbol->string sym1) (symbol->string sym2)))
      (lambda (_sym10 _sym20)
         (string<? (symbol->string _sym10) (symbol->string _sym20)))))
 
(define insert (<change>
      (lambda (sym S)
         (if (not (pair? S))
            (list sym)
            (if (eq? sym (car S))
               S
               (if (symbol<? sym (car S))
                  (cons sym S)
                  (cons (car S) (insert sym (cdr S)))))))
      (lambda (_sym0 _S0)
         (if (not (pair? _S0))
            (list _sym0)
            (if (eq? _sym0 (car _S0))
               _S0
               (if (symbol<? _sym0 (car _S0))
                  (cons _sym0 _S0)
                  (cons (car _S0) (insert _sym0 (cdr _S0)))))))))
 
(define remove (<change>
      (lambda (sym S)
         (if (not (pair? S))
            ()
            (if (eq? (car S) sym)
               (cdr S)
               (cons (car S) (remove sym (cdr S))))))
      (lambda (_sym0 _S0)
         (if (not (pair? _S0))
            ()
            (if (eq? (car _S0) _sym0)
               (cdr _S0)
               (cons (car _S0) (remove _sym0 (cdr _S0))))))))
 
(define union (<change>
      (lambda (set1 set2)
         (if (not (pair? set1))
            set2
            (insert (car set1) (union (cdr set1) set2))))
      (lambda (_set10 _set20)
         (if (not (pair? _set10))
            _set20
            (insert (car _set10) (union (cdr _set10) _set20))))))
 
(define difference (<change>
      (lambda (set1 set2)
         (if (not (pair? set2))
            set1
            (difference (remove (car set2) set1) (cdr set2))))
      (lambda (_set10 _set20)
         (if (not (pair? _set20))
            _set10
            (difference (remove (car _set20) _set10) (cdr _set20))))))
 
(define reduce (<change>
      (lambda (f lst init)
         (if (not (pair? lst))
            init
            (reduce f (cdr lst) (f (car lst) init))))
      (lambda (_f0 _lst0 _init0)
         (if (not (pair? _lst0))
            _init0
            (reduce _f0 (cdr _lst0) (_f0 (car _lst0) _init0))))))
 
(define azip (lambda (list1 list2)
      (if (if (pair? list1) (pair? list2) #f)
         (cons (list (car list1) (car list2)) (azip (cdr list1) (cdr list2)))
         ())))
 
(define assq-remove-key (<change>
      (lambda (env key)
         (if (not (pair? env))
            ()
            (if (eq? (car (car env)) key)
               (assq-remove-key (cdr env) key)
               (cons (car env) (assq-remove-key (cdr env) key)))))
      (lambda (_env0 _key0)
         (if (not (pair? _env0))
            ()
            (if (eq? (car (car _env0)) _key0)
               (assq-remove-key (cdr _env0) _key0)
               (cons (car _env0) (assq-remove-key (cdr _env0) _key0)))))))
 
(define assq-remove-keys (<change>
      (lambda (env keys)
         (if (not (pair? keys))
            env
            (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))
      (lambda (_env0 _keys0)
         (if (not (pair? _keys0))
            _env0
            (assq-remove-keys (assq-remove-key _env0 (car _keys0)) (cdr _keys0))))))
 
(define const? (lambda (exp)
      (let ((__or_res (integer? exp)))
         (if __or_res __or_res (boolean? exp)))))
 
(define ref? (<change>
      (lambda (exp)
         (symbol? exp))
      (lambda (_exp0)
         (symbol? _exp0))))
 
(define let? (<change>
      (lambda (exp)
         (tagged-list? 'let exp))
      (lambda (_exp0)
         (tagged-list? 'let _exp0))))
 
(define let->bindings (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define let->exp (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define let->bound-vars (<change>
      (lambda (exp)
         (map car (cadr exp)))
      (lambda (_exp0)
         (map car (cadr _exp0)))))
 
(define let->args (<change>
      (lambda (exp)
         (map cadr (cadr exp)))
      (lambda (_exp0)
         (map cadr (cadr _exp0)))))
 
(define letrec? (<change>
      (lambda (exp)
         (tagged-list? 'letrec exp))
      (lambda (_exp0)
         (tagged-list? 'letrec _exp0))))
 
(define letrec->bindings (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define letrec->exp (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define letrec->bound-vars (lambda (exp)
      (map car (cadr exp))))
 
(define letrec->args (<change>
      (lambda (exp)
         (map cadr (cadr exp)))
      (lambda (_exp0)
         (map cadr (cadr _exp0)))))
 
(define lambda? (lambda (exp)
      (tagged-list? 'lambda exp)))
 
(define lambda->formals (lambda (exp)
      (cadr exp)))
 
(define lambda->exp (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define if? (lambda (exp)
      (tagged-list? 'if exp)))
 
(define if->condition (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define if->then (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define if->else (<change>
      (lambda (exp)
         (cadddr exp))
      (lambda (_exp0)
         (cadddr _exp0))))
 
(define app? (<change>
      (lambda (exp)
         (pair? exp))
      (lambda (_exp0)
         (pair? _exp0))))
 
(define app->fun (<change>
      (lambda (exp)
         (car exp))
      (lambda (_exp0)
         (car _exp0))))
 
(define app->args (<change>
      (lambda (exp)
         (cdr exp))
      (lambda (_exp0)
         (cdr _exp0))))
 
(define prim? (lambda (exp)
      (<change>
         (let ((__or_res (eq? exp '+)))
            (if __or_res
               __or_res
               (let ((__or_res (eq? exp '-)))
                  (if __or_res
                     __or_res
                     (let ((__or_res (eq? exp '*)))
                        (if __or_res
                           __or_res
                           (let ((__or_res (eq? exp '=)))
                              (if __or_res __or_res (eq? exp 'display)))))))))
         (let ((___or_res0 (eq? exp '+)))
            (if ___or_res0
               ___or_res0
               (let ((___or_res1 (eq? exp '-)))
                  (if ___or_res1
                     ___or_res1
                     (let ((___or_res2 (eq? exp '*)))
                        (if ___or_res2
                           ___or_res2
                           (let ((___or_res3 (eq? exp '=)))
                              (if ___or_res3 ___or_res3 (eq? exp 'display))))))))))))
 
(define begin? (lambda (exp)
      (tagged-list? 'begin exp)))
 
(define begin->exps (<change>
      (lambda (exp)
         (cdr exp))
      (lambda (_exp0)
         (cdr _exp0))))
 
(define set!? (lambda (exp)
      (tagged-list? 'set! exp)))
 
(define set!->var (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define set!->exp (lambda (exp)
      (caddr exp)))
 
(define closure? (lambda (exp)
      (tagged-list? 'closure exp)))
 
(define closure->lam (lambda (exp)
      (cadr exp)))
 
(define closure->env (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define env-make? (lambda (exp)
      (tagged-list? 'env-make exp)))
 
(define env-make->id (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define env-make->fields (<change>
      (lambda (exp)
         (map car (cddr exp)))
      (lambda (_exp0)
         (map car (cddr _exp0)))))
 
(define env-make->values (lambda (exp)
      (map cadr (cddr exp))))
 
(define env-get? (<change>
      (lambda (exp)
         (tagged-list? 'env-get exp))
      (lambda (_exp0)
         (tagged-list? 'env-get _exp0))))
 
(define env-get->id (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define env-get->field (lambda (exp)
      (caddr exp)))
 
(define env-get->env (lambda (exp)
      (cadddr exp)))
 
(define set-cell!? (<change>
      (lambda (exp)
         (tagged-list? 'set-cell! exp))
      (lambda (_exp0)
         (tagged-list? 'set-cell! _exp0))))
 
(define set-cell!->cell (lambda (exp)
      (cadr exp)))
 
(define set-cell!->value (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define cell? (lambda (exp)
      (tagged-list? 'cell exp)))
 
(define cell->value (lambda (exp)
      (cadr exp)))
 
(define cell-get? (lambda (exp)
      (tagged-list? 'cell-get exp)))
 
(define cell-get->cell (lambda (exp)
      (cadr exp)))
 
(define substitute-var (<change>
      (lambda (env var)
         (let ((sub (assq var env)))
            (if sub (cadr sub) var)))
      (lambda (_env0 _var0)
         (let ((_sub0 (assq _var0 _env0)))
            (if _sub0 (cadr _sub0) _var0)))))
 
(define substitute (lambda (env exp)
      (define substitute-with (lambda (env)
            (<change>
               (lambda (exp)
                  (substitute env exp))
               (lambda (_exp0)
                  (substitute env _exp0)))))
      (if (null? env)
         exp
         (if (const? exp)
            exp
            (if (prim? exp)
               exp
               (if (ref? exp)
                  (substitute-var env exp)
                  (if (lambda? exp)
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (lambda->formals exp)
                           (__toplevel_cons (substitute (assq-remove-keys env (lambda->formals exp)) (lambda->exp exp)) ())))
                     (if (set!? exp)
                        (__toplevel_cons
                           'set!
                           (__toplevel_cons
                              (substitute-var env (set!->var exp))
                              (__toplevel_cons (substitute env (set!->exp exp)) ())))
                        (if (if? exp)
                           (__toplevel_cons
                              'if
                              (__toplevel_cons
                                 (substitute env (if->condition exp))
                                 (__toplevel_cons
                                    (substitute env (if->then exp))
                                    (__toplevel_cons (substitute env (if->else exp)) ()))))
                           (if (let? exp)
                              (__toplevel_cons
                                 'let
                                 (__toplevel_cons
                                    (azip (let->bound-vars exp) (map (substitute-with env) (let->args exp)))
                                    (__toplevel_cons (substitute (assq-remove-keys env (let->bound-vars exp)) (let->exp exp)) ())))
                              (if (letrec? exp)
                                 (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
                                    (__toplevel_cons
                                       'letrec
                                       (__toplevel_cons
                                          (azip (letrec->bound-vars exp) (map (substitute-with new-env) (letrec->args exp)))
                                          (__toplevel_cons (substitute new-env (letrec->exp exp)) ()))))
                                 (if (begin? exp)
                                    (cons 'begin (map (substitute-with env) (begin->exps exp)))
                                    (if (cell? exp)
                                       (__toplevel_cons 'cell (__toplevel_cons (substitute env (cell->value exp)) ()))
                                       (if (cell-get? exp)
                                          (__toplevel_cons 'cell-get (__toplevel_cons (substitute env (cell-get->cell exp)) ()))
                                          (if (set-cell!? exp)
                                             (__toplevel_cons
                                                'set-cell!
                                                (__toplevel_cons
                                                   (substitute env (set-cell!->cell exp))
                                                   (__toplevel_cons (substitute env (set-cell!->value exp)) ())))
                                             (if (closure? exp)
                                                (__toplevel_cons
                                                   'closure
                                                   (__toplevel_cons
                                                      (substitute env (closure->lam exp))
                                                      (__toplevel_cons (substitute env (closure->env exp)) ())))
                                                (if (env-make? exp)
                                                   (__toplevel_cons
                                                      'env-make
                                                      (__toplevel_cons
                                                         (env-make->id exp)
                                                         (__toplevel_append
                                                            (azip (env-make->fields exp) (map (substitute-with env) (env-make->values exp)))
                                                            ())))
                                                   (if (env-get? exp)
                                                      (__toplevel_cons
                                                         'env-get
                                                         (__toplevel_cons
                                                            (env-get->id exp)
                                                            (__toplevel_cons (env-get->field exp) (__toplevel_cons (substitute env (env-get->env exp)) ()))))
                                                      (if (app? exp)
                                                         (map (substitute-with env) exp)
                                                         (error "unhandled expression type in substitution: " exp))))))))))))))))))))
 
(define let=>lambda (<change>
      (lambda (exp)
         (if (let? exp)
            (let ((vars (map car (let->bindings exp)))
                  (args (map cadr (let->bindings exp))))
               (__toplevel_cons
                  (__toplevel_cons
                     'lambda
                     (__toplevel_cons (__toplevel_append vars ()) (__toplevel_cons (let->exp exp) ())))
                  (__toplevel_append args ())))
            exp))
      (lambda (_exp0)
         (if (let? _exp0)
            (let ((_vars0 (map car (let->bindings _exp0)))
                  (_args0 (map cadr (let->bindings _exp0))))
               (__toplevel_cons
                  (__toplevel_cons
                     'lambda
                     (__toplevel_cons (__toplevel_append _vars0 ()) (__toplevel_cons (let->exp _exp0) ())))
                  (__toplevel_append _args0 ())))
            _exp0))))
 
(define letrec=>lets+sets (lambda (exp)
      (if (letrec? exp)
         (let* ((bindings (letrec->bindings exp))
                (namings (map (<change> (lambda (b) (list (car b) #f)) (lambda (_b0) (list (car _b0) #f))) bindings))
                (names (letrec->bound-vars exp))
                (sets (map
                        (<change>
                           (lambda (binding)
                              (cons 'set! binding))
                           (lambda (_binding0)
                              (cons 'set! _binding0)))
                        bindings))
                (args (letrec->args exp)))
            (__toplevel_cons
               'let
               (__toplevel_cons
                  namings
                  (__toplevel_cons
                     (__toplevel_cons 'begin (__toplevel_append (append sets (list (letrec->exp exp))) ()))
                     ()))))
         #f)))
 
(define begin=>let (<change>
      (lambda (exp)
         (define singlet? (lambda (l)
               (if (list? l) (= (length l) 1) #f)))
         (define dummy-bind (lambda (exps)
               (if (singlet? exps)
                  (car exps)
                  (if (pair? exps)
                     (__toplevel_cons
                        'let
                        (__toplevel_cons
                           (__toplevel_cons (__toplevel_cons '$_ (__toplevel_cons (car exps) ())) ())
                           (__toplevel_cons (dummy-bind (cdr exps)) ())))
                     #f))))
         (dummy-bind (begin->exps exp)))
      (lambda (_exp0)
         (define singlet? (lambda (_l0)
               (if (list? _l0) (= (length _l0) 1) #f)))
         (define dummy-bind (lambda (_exps0)
               (if (singlet? _exps0)
                  (car _exps0)
                  (if (pair? _exps0)
                     (__toplevel_cons
                        'let
                        (__toplevel_cons
                           (__toplevel_cons (__toplevel_cons '$_ (__toplevel_cons (car _exps0) ())) ())
                           (__toplevel_cons (dummy-bind (cdr _exps0)) ())))
                     #f))))
         (dummy-bind (begin->exps _exp0)))))
 
(define desugar (lambda (exp)
      (if (const? exp)
         exp
         (if (prim? exp)
            exp
            (if (ref? exp)
               exp
               (if (lambda? exp)
                  (__toplevel_cons
                     'lambda
                     (__toplevel_cons (lambda->formals exp) (__toplevel_cons (desugar (lambda->exp exp)) ())))
                  (if (set!? exp)
                     (__toplevel_cons 'set! (__toplevel_cons (set!->var exp) (__toplevel_cons (set!->exp exp) ())))
                     (if (if? exp)
                        (__toplevel_cons
                           'if
                           (__toplevel_cons
                              (if->condition exp)
                              (__toplevel_cons (if->then exp) (__toplevel_cons (if->else exp) ()))))
                        (if (let? exp)
                           (desugar (let=>lambda exp))
                           (if (letrec? exp)
                              (desugar (letrec=>lets+sets exp))
                              (if (begin? exp)
                                 (desugar (begin=>let exp))
                                 (if (cell? exp)
                                    (__toplevel_cons 'cell (__toplevel_cons (desugar (cell->value exp)) ()))
                                    (if (cell-get? exp)
                                       (__toplevel_cons 'cell-get (__toplevel_cons (desugar (cell-get->cell exp)) ()))
                                       (if (set-cell!? exp)
                                          (__toplevel_cons
                                             'set-cell!
                                             (__toplevel_cons
                                                (desugar (set-cell!->cell exp))
                                                (__toplevel_cons (desugar (set-cell!->value exp)) ())))
                                          (if (closure? exp)
                                             (__toplevel_cons
                                                'closure
                                                (__toplevel_cons (desugar (closure->lam exp)) (__toplevel_cons (desugar (closure->env exp)) ())))
                                             (if (env-make? exp)
                                                (__toplevel_cons
                                                   'env-make
                                                   (__toplevel_cons
                                                      (env-make->id exp)
                                                      (__toplevel_append (azip (env-make->fields exp) (map desugar (env-make->values exp))) ())))
                                                (if (env-get? exp)
                                                   (__toplevel_cons
                                                      'env-get
                                                      (__toplevel_cons
                                                         (env-get->id exp)
                                                         (__toplevel_cons (env-get->field exp) (__toplevel_cons (env-get->env exp) ()))))
                                                   (if (app? exp)
                                                      (map desugar exp)
                                                      (error "unknown exp: " exp)))))))))))))))))))
 
(define free-vars (<change>
      (lambda (exp)
         (if (const? exp)
            ()
            (if (prim? exp)
               ()
               (if (ref? exp)
                  (list exp)
                  (if (lambda? exp)
                     (difference (free-vars (lambda->exp exp)) (lambda->formals exp))
                     (if (if? exp)
                        (union
                           (free-vars (if->condition exp))
                           (union (free-vars (if->then exp)) (free-vars (if->else exp))))
                        (if (set!? exp)
                           (union (list (set!->var exp)) (free-vars (set!->exp exp)))
                           (if (let? exp)
                              (free-vars (let=>lambda exp))
                              (if (letrec? exp)
                                 not-handled
                                 (if (begin? exp)
                                    (reduce union (map free-vars (begin->exps exp)) ())
                                    (if (cell-get? exp)
                                       (free-vars (cell-get->cell exp))
                                       (if (cell? exp)
                                          (free-vars (cell->value exp))
                                          (if (set-cell!? exp)
                                             (union (free-vars (set-cell!->cell exp)) (free-vars (set-cell!->value exp)))
                                             (if (closure? exp)
                                                (union (free-vars (closure->lam exp)) (free-vars (closure->env exp)))
                                                (if (env-make? exp)
                                                   (reduce union (map free-vars (env-make->values exp)) ())
                                                   (if (env-get? exp)
                                                      (free-vars (env-get->env exp))
                                                      (if (app? exp)
                                                         (reduce union (map free-vars exp) ())
                                                         (error "unknown expression: " exp))))))))))))))))))
      (lambda (_exp0)
         (if (const? _exp0)
            ()
            (if (prim? _exp0)
               ()
               (if (ref? _exp0)
                  (list _exp0)
                  (if (lambda? _exp0)
                     (difference (free-vars (lambda->exp _exp0)) (lambda->formals _exp0))
                     (if (if? _exp0)
                        (union
                           (free-vars (if->condition _exp0))
                           (union (free-vars (if->then _exp0)) (free-vars (if->else _exp0))))
                        (if (set!? _exp0)
                           (union (list (set!->var _exp0)) (free-vars (set!->exp _exp0)))
                           (if (let? _exp0)
                              (free-vars (let=>lambda _exp0))
                              (if (letrec? _exp0)
                                 not-handled
                                 (if (begin? _exp0)
                                    (reduce union (map free-vars (begin->exps _exp0)) ())
                                    (if (cell-get? _exp0)
                                       (free-vars (cell-get->cell _exp0))
                                       (if (cell? _exp0)
                                          (free-vars (cell->value _exp0))
                                          (if (set-cell!? _exp0)
                                             (union (free-vars (set-cell!->cell _exp0)) (free-vars (set-cell!->value _exp0)))
                                             (if (closure? _exp0)
                                                (union (free-vars (closure->lam _exp0)) (free-vars (closure->env _exp0)))
                                                (if (env-make? _exp0)
                                                   (reduce union (map free-vars (env-make->values _exp0)) ())
                                                   (if (env-get? _exp0)
                                                      (free-vars (env-get->env _exp0))
                                                      (if (app? _exp0)
                                                         (reduce union (map free-vars _exp0) ())
                                                         (error "unknown expression: " _exp0))))))))))))))))))))
 
(define mutable-variables ())
 
(define mark-mutable (lambda (symbol)
      (set! mutable-variables (cons symbol mutable-variables))))
 
(define is-mutable? (lambda (symbol)
      (define is-in? (<change>
            (lambda (S)
               (if (not (pair? S))
                  #f
                  (if (eq? (car S) symbol) #t (is-in? (cdr S)))))
            (lambda (_S0)
               (if (not (pair? _S0))
                  #f
                  (if (eq? (car _S0) symbol) #t (is-in? (cdr _S0)))))))
      (is-in? mutable-variables)))
 
(define analyze-mutable-variables (lambda (exp)
      (if (const? exp)
         (void)
         (if (prim? exp)
            (void)
            (if (ref? exp)
               (void)
               (if (lambda? exp)
                  (analyze-mutable-variables (lambda->exp exp))
                  (if (set!? exp)
                     (begin
                        (mark-mutable (set!->var exp))
                        (analyze-mutable-variables (set!->exp exp)))
                     (if (if? exp)
                        (begin
                           (analyze-mutable-variables (if->condition exp))
                           (analyze-mutable-variables (if->then exp))
                           (analyze-mutable-variables (if->else exp)))
                        (if (let? exp)
                           (begin
                              (map analyze-mutable-variables (map cadr (let->bindings exp)))
                              (analyze-mutable-variables (let->exp exp)))
                           (if (letrec? exp)
                              (begin
                                 (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                                 (analyze-mutable-variables (letrec->exp exp)))
                              (if (begin? exp)
                                 (begin
                                    (map analyze-mutable-variables (begin->exps exp))
                                    (void))
                                 (if (app? exp)
                                    (begin
                                       (map analyze-mutable-variables exp)
                                       (void))
                                    (error "unknown expression type: " exp)))))))))))))
 
(define wrap-mutables (<change>
      (lambda (exp)
         (define wrap-mutable-formals (lambda (formals body-exp)
               (if (not (pair? formals))
                  body-exp
                  (if (is-mutable? (car formals))
                     (__toplevel_cons
                        'let
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons
                                 (car formals)
                                 (__toplevel_cons (__toplevel_cons 'cell (__toplevel_cons (car formals) ())) ()))
                              ())
                           (__toplevel_cons (wrap-mutable-formals (cdr formals) body-exp) ())))
                     (wrap-mutable-formals (cdr formals) body-exp)))))
         (if (const? exp)
            exp
            (if (ref? exp)
               (if (is-mutable? exp)
                  (__toplevel_cons 'cell-get (__toplevel_cons exp ()))
                  exp)
               (if (prim? exp)
                  exp
                  (if (lambda? exp)
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (lambda->formals exp)
                           (__toplevel_cons (wrap-mutable-formals (lambda->formals exp) (wrap-mutables (lambda->exp exp))) ())))
                     (if (set!? exp)
                        (__toplevel_cons
                           'set-cell!
                           (__toplevel_cons (set!->var exp) (__toplevel_cons (wrap-mutables (set!->exp exp)) ())))
                        (if (if? exp)
                           (__toplevel_cons
                              'if
                              (__toplevel_cons
                                 (wrap-mutables (if->condition exp))
                                 (__toplevel_cons
                                    (wrap-mutables (if->then exp))
                                    (__toplevel_cons (wrap-mutables (if->else exp)) ()))))
                           (if (app? exp)
                              (map wrap-mutables exp)
                              (error "unknown expression type: " exp)))))))))
      (lambda (_exp0)
         (define wrap-mutable-formals (lambda (_formals0 _body-exp0)
               (if (not (pair? _formals0))
                  _body-exp0
                  (if (is-mutable? (car _formals0))
                     (__toplevel_cons
                        'let
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons
                                 (car _formals0)
                                 (__toplevel_cons (__toplevel_cons 'cell (__toplevel_cons (car _formals0) ())) ()))
                              ())
                           (__toplevel_cons (wrap-mutable-formals (cdr _formals0) _body-exp0) ())))
                     (wrap-mutable-formals (cdr _formals0) _body-exp0)))))
         (if (const? _exp0)
            _exp0
            (if (ref? _exp0)
               (if (is-mutable? _exp0)
                  (__toplevel_cons 'cell-get (__toplevel_cons _exp0 ()))
                  _exp0)
               (if (prim? _exp0)
                  _exp0
                  (if (lambda? _exp0)
                     (__toplevel_cons
                        'lambda
                        (__toplevel_cons
                           (lambda->formals _exp0)
                           (__toplevel_cons
                              (wrap-mutable-formals (lambda->formals _exp0) (wrap-mutables (lambda->exp _exp0)))
                              ())))
                     (if (set!? _exp0)
                        (__toplevel_cons
                           'set-cell!
                           (__toplevel_cons (set!->var _exp0) (__toplevel_cons (wrap-mutables (set!->exp _exp0)) ())))
                        (if (if? _exp0)
                           (__toplevel_cons
                              'if
                              (__toplevel_cons
                                 (wrap-mutables (if->condition _exp0))
                                 (__toplevel_cons
                                    (wrap-mutables (if->then _exp0))
                                    (__toplevel_cons (wrap-mutables (if->else _exp0)) ()))))
                           (if (app? _exp0)
                              (map wrap-mutables _exp0)
                              (error "unknown expression type: " _exp0)))))))))))
 
(define mangle (lambda (symbol)
      (define m (lambda (chars)
            (if (null? chars)
               ()
               (if (<change> (let ((__or_res (if (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)) #f))) (if __or_res __or_res (char-numeric? (car chars)))) (let ((___or_res0 (if (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)) #f))) (if ___or_res0 ___or_res0 (char-numeric? (car chars)))))
                  (cons (car chars) (m (cdr chars)))
                  (cons #\_ (append (integer->char-list (char->natural (car chars))) (m (cdr chars))))))))
      (list->string (m (string->list (symbol->string symbol))))))
 
(define num-environments 0)
 
(define environments ())
 
(define allocate-environment (<change>
      (lambda (fields)
         (let ((id num-environments))
            (set! num-environments (+ 1 num-environments))
            (set! environments (cons (cons id fields) environments))
            id))
      (lambda (_fields0)
         (let ((_id0 num-environments))
            (set! num-environments (+ 1 num-environments))
            (set! environments (cons (cons _id0 _fields0) environments))
            _id0))))
 
(define get-environment (<change>
      (lambda (id)
         (cdr (assv id environments)))
      (lambda (_id0)
         (cdr (assv _id0 environments)))))
 
(define closure-convert (<change>
      (lambda (exp)
         (if (const? exp)
            exp
            (if (prim? exp)
               exp
               (if (ref? exp)
                  exp
                  (if (lambda? exp)
                     (let* (($env (gensym 'env))
                            (body (closure-convert (lambda->exp exp)))
                            (fv (difference (free-vars body) (lambda->formals exp)))
                            (id (allocate-environment fv))
                            (sub (map
                                   (lambda (v)
                                      (list
                                         v
                                         (__toplevel_cons 'env-get (__toplevel_cons id (__toplevel_cons v (__toplevel_cons $env ()))))))
                                   fv)))
                        (__toplevel_cons
                           'closure
                           (__toplevel_cons
                              (__toplevel_cons
                                 'lambda
                                 (__toplevel_cons
                                    (__toplevel_cons $env (__toplevel_append (lambda->formals exp) ()))
                                    (__toplevel_cons (substitute sub body) ())))
                              (__toplevel_cons
                                 (__toplevel_cons 'env-make (__toplevel_cons id (__toplevel_append (azip fv fv) ())))
                                 ()))))
                     (if (if? exp)
                        (__toplevel_cons
                           'if
                           (__toplevel_cons
                              (closure-convert (if->condition exp))
                              (__toplevel_cons
                                 (closure-convert (if->then exp))
                                 (__toplevel_cons (closure-convert (if->else exp)) ()))))
                        (if (set!? exp)
                           (__toplevel_cons
                              'set!
                              (__toplevel_cons (set!->var exp) (__toplevel_cons (closure-convert (set!->exp exp)) ())))
                           (if (cell? exp)
                              (__toplevel_cons 'cell (__toplevel_cons (closure-convert (cell->value exp)) ()))
                              (if (cell-get? exp)
                                 (__toplevel_cons 'cell-get (__toplevel_cons (closure-convert (cell-get->cell exp)) ()))
                                 (if (set-cell!? exp)
                                    (__toplevel_cons
                                       'set-cell!
                                       (__toplevel_cons
                                          (closure-convert (set-cell!->cell exp))
                                          (__toplevel_cons (closure-convert (set-cell!->value exp)) ())))
                                    (if (app? exp)
                                       (map closure-convert exp)
                                       (error "unhandled exp: " exp))))))))))))
      (lambda (_exp0)
         (if (const? _exp0)
            _exp0
            (if (prim? _exp0)
               _exp0
               (if (ref? _exp0)
                  _exp0
                  (if (lambda? _exp0)
                     (let* ((_$env0 (gensym 'env))
                            (_body0 (closure-convert (lambda->exp _exp0)))
                            (_fv0 (difference (free-vars _body0) (lambda->formals _exp0)))
                            (_id0 (allocate-environment _fv0))
                            (_sub0 (map
                                     (lambda (_v0)
                                        (list
                                           _v0
                                           (__toplevel_cons 'env-get (__toplevel_cons _id0 (__toplevel_cons _v0 (__toplevel_cons _$env0 ()))))))
                                     _fv0)))
                        (__toplevel_cons
                           'closure
                           (__toplevel_cons
                              (__toplevel_cons
                                 'lambda
                                 (__toplevel_cons
                                    (__toplevel_cons _$env0 (__toplevel_append (lambda->formals _exp0) ()))
                                    (__toplevel_cons (substitute _sub0 _body0) ())))
                              (__toplevel_cons
                                 (__toplevel_cons 'env-make (__toplevel_cons _id0 (__toplevel_append (azip _fv0 _fv0) ())))
                                 ()))))
                     (if (if? _exp0)
                        (__toplevel_cons
                           'if
                           (__toplevel_cons
                              (closure-convert (if->condition _exp0))
                              (__toplevel_cons
                                 (closure-convert (if->then _exp0))
                                 (__toplevel_cons (closure-convert (if->else _exp0)) ()))))
                        (if (set!? _exp0)
                           (__toplevel_cons
                              'set!
                              (__toplevel_cons (set!->var _exp0) (__toplevel_cons (closure-convert (set!->exp _exp0)) ())))
                           (if (cell? _exp0)
                              (__toplevel_cons 'cell (__toplevel_cons (closure-convert (cell->value _exp0)) ()))
                              (if (cell-get? _exp0)
                                 (__toplevel_cons 'cell-get (__toplevel_cons (closure-convert (cell-get->cell _exp0)) ()))
                                 (if (set-cell!? _exp0)
                                    (__toplevel_cons
                                       'set-cell!
                                       (__toplevel_cons
                                          (closure-convert (set-cell!->cell _exp0))
                                          (__toplevel_cons (closure-convert (set-cell!->value _exp0)) ())))
                                    (if (app? _exp0)
                                       (map closure-convert _exp0)
                                       (error "unhandled exp: " _exp0))))))))))))))
 
(define c-compile-program (<change>
      (lambda (exp)
         (let* ((preamble "")
                (append-preamble (lambda (s)
                                   (set! preamble (string-append preamble "  " s "\n"))))
                (body (c-compile-exp exp append-preamble)))
            (string-append
               "int main (int argc, char* argv[]) {\n"
               preamble
               "  __sum         = MakePrimitive(__prim_sum) ;\n"
               "  __product     = MakePrimitive(__prim_product) ;\n"
               "  __difference  = MakePrimitive(__prim_difference) ;\n"
               "  __display     = MakePrimitive(__prim_display) ;\n"
               "  __numEqual    = MakePrimitive(__prim_numEqual) ;\n"
               "  "
               body
               " ;\n"
               "  return 0;\n"
               " }\n")))
      (lambda (_exp0)
         (let* ((_preamble0 "")
                (_append-preamble0 (lambda (_s0)
                                     (set! _preamble0 (string-append _preamble0 "  " _s0 "\n"))))
                (_body0 (c-compile-exp _exp0 _append-preamble0)))
            (string-append
               "int main (int argc, char* argv[]) {\n"
               _preamble0
               "  __sum         = MakePrimitive(__prim_sum) ;\n"
               "  __product     = MakePrimitive(__prim_product) ;\n"
               "  __difference  = MakePrimitive(__prim_difference) ;\n"
               "  __display     = MakePrimitive(__prim_display) ;\n"
               "  __numEqual    = MakePrimitive(__prim_numEqual) ;\n"
               "  "
               _body0
               " ;\n"
               "  return 0;\n"
               " }\n")))))
 
(define c-compile-exp (<change>
      (lambda (exp append-preamble)
         (if (const? exp)
            (c-compile-const exp)
            (if (prim? exp)
               (c-compile-prim exp)
               (if (ref? exp)
                  (c-compile-ref exp)
                  (if (if? exp)
                     (c-compile-if exp append-preamble)
                     (if (cell? exp)
                        (c-compile-cell exp append-preamble)
                        (if (cell-get? exp)
                           (c-compile-cell-get exp append-preamble)
                           (if (set-cell!? exp)
                              (c-compile-set-cell! exp append-preamble)
                              (if (closure? exp)
                                 (c-compile-closure exp append-preamble)
                                 (if (env-make? exp)
                                    (c-compile-env-make exp append-preamble)
                                    (if (env-get? exp)
                                       (c-compile-env-get exp append-preamble)
                                       (if (app? exp)
                                          (c-compile-app exp append-preamble)
                                          (error "unknown exp in c-compile-exp: " exp)))))))))))))
      (lambda (_exp0 _append-preamble0)
         (if (const? _exp0)
            (c-compile-const _exp0)
            (if (prim? _exp0)
               (c-compile-prim _exp0)
               (if (ref? _exp0)
                  (c-compile-ref _exp0)
                  (if (if? _exp0)
                     (c-compile-if _exp0 _append-preamble0)
                     (if (cell? _exp0)
                        (c-compile-cell _exp0 _append-preamble0)
                        (if (cell-get? _exp0)
                           (c-compile-cell-get _exp0 _append-preamble0)
                           (if (set-cell!? _exp0)
                              (c-compile-set-cell! _exp0 _append-preamble0)
                              (if (closure? _exp0)
                                 (c-compile-closure _exp0 _append-preamble0)
                                 (if (env-make? _exp0)
                                    (c-compile-env-make _exp0 _append-preamble0)
                                    (if (env-get? _exp0)
                                       (c-compile-env-get _exp0 _append-preamble0)
                                       (if (app? _exp0)
                                          (c-compile-app _exp0 _append-preamble0)
                                          (error "unknown exp in c-compile-exp: " _exp0)))))))))))))))
 
(define c-compile-const (<change>
      (lambda (exp)
         (if (integer? exp)
            (string-append "MakeInt(" (number->string exp) ")")
            (if (boolean? exp)
               (string-append "MakeBoolean(" (if exp "1" "0") ")")
               (error "unknown constant: " exp))))
      (lambda (_exp0)
         (if (integer? _exp0)
            (string-append "MakeInt(" (number->string _exp0) ")")
            (if (boolean? _exp0)
               (string-append "MakeBoolean(" (if _exp0 "1" "0") ")")
               (error "unknown constant: " _exp0))))))
 
(define c-compile-prim (<change>
      (lambda (p)
         (if (eq? '+ p)
            "__sum"
            (if (eq? '- p)
               "__difference"
               (if (eq? '* p)
                  "__product"
                  (if (eq? '= p)
                     "__numEqual"
                     (if (eq? 'display p)
                        "__display"
                        (error "unhandled primitive: " p)))))))
      (lambda (_p0)
         (if (eq? '+ _p0)
            "__sum"
            (if (eq? '- _p0)
               "__difference"
               (if (eq? '* _p0)
                  "__product"
                  (if (eq? '= _p0)
                     "__numEqual"
                     (if (eq? 'display _p0)
                        "__display"
                        (error "unhandled primitive: " _p0)))))))))
 
(define c-compile-ref (lambda (exp)
      (mangle exp)))
 
(define c-compile-args (lambda (args append-preamble)
      (if (not (pair? args))
         ""
         (string-append
            (c-compile-exp (car args) append-preamble)
            (if (pair? (cdr args))
               (string-append ", " (c-compile-args (cdr args) append-preamble))
               "")))))
 
(define c-compile-app (<change>
      (lambda (exp append-preamble)
         (let (($tmp (mangle (gensym 'tmp))))
            (append-preamble (string-append "Value " $tmp " ; "))
            (let* ((args (app->args exp))
                   (fun (app->fun exp)))
               (string-append
                  "("
                  $tmp
                  " = "
                  (c-compile-exp fun append-preamble)
                  ","
                  $tmp
                  ".clo.lam("
                  "MakeEnv("
                  $tmp
                  ".clo.env)"
                  (if (null? args) "" ",")
                  (c-compile-args args append-preamble)
                  "))"))))
      (lambda (_exp0 _append-preamble0)
         (let ((_$tmp0 (mangle (gensym 'tmp))))
            (_append-preamble0 (string-append "Value " _$tmp0 " ; "))
            (let* ((_args0 (app->args _exp0))
                   (_fun0 (app->fun _exp0)))
               (string-append
                  "("
                  _$tmp0
                  " = "
                  (c-compile-exp _fun0 _append-preamble0)
                  ","
                  _$tmp0
                  ".clo.lam("
                  "MakeEnv("
                  _$tmp0
                  ".clo.env)"
                  (if (null? _args0) "" ",")
                  (c-compile-args _args0 _append-preamble0)
                  "))"))))))
 
(define c-compile-if (<change>
      (lambda (exp append-preamble)
         (string-append
            "("
            (c-compile-exp (if->condition exp) append-preamble)
            ").b.value ? "
            "("
            (c-compile-exp (if->then exp) append-preamble)
            ") : "
            "("
            (c-compile-exp (if->else exp) append-preamble)
            ")"))
      (lambda (_exp0 _append-preamble0)
         (string-append
            "("
            (c-compile-exp (if->condition _exp0) _append-preamble0)
            ").b.value ? "
            "("
            (c-compile-exp (if->then _exp0) _append-preamble0)
            ") : "
            "("
            (c-compile-exp (if->else _exp0) _append-preamble0)
            ")"))))
 
(define c-compile-set-cell! (lambda (exp append-preamble)
      (string-append
         "(*"
         "("
         (c-compile-exp (set-cell!->cell exp) append-preamble)
         ".cell.addr)"
         " = "
         (c-compile-exp (set-cell!->value exp) append-preamble)
         ")")))
 
(define c-compile-cell-get (<change>
      (lambda (exp append-preamble)
         (string-append "(*(" (c-compile-exp (cell-get->cell exp) append-preamble) ".cell.addr" "))"))
      (lambda (_exp0 _append-preamble0)
         (string-append "(*(" (c-compile-exp (cell-get->cell _exp0) _append-preamble0) ".cell.addr" "))"))))
 
(define c-compile-cell (lambda (exp append-preamble)
      (string-append "NewCell(" (c-compile-exp (cell->value exp) append-preamble) ")")))
 
(define c-compile-env-make (<change>
      (lambda (exp append-preamble)
         (string-append
            "MakeEnv(__alloc_env"
            (number->string (env-make->id exp))
            "("
            (c-compile-args (env-make->values exp) append-preamble)
            "))"))
      (lambda (_exp0 _append-preamble0)
         (string-append
            "MakeEnv(__alloc_env"
            (number->string (env-make->id _exp0))
            "("
            (c-compile-args (env-make->values _exp0) _append-preamble0)
            "))"))))
 
(define c-compile-env-get (<change>
      (lambda (exp append-preamble)
         (string-append
            "((struct __env_"
            (number->string (env-get->id exp))
            "*)"
            (c-compile-exp (env-get->env exp) append-preamble)
            ".env.env)->"
            (mangle (env-get->field exp))))
      (lambda (_exp0 _append-preamble0)
         (string-append
            "((struct __env_"
            (number->string (env-get->id _exp0))
            "*)"
            (c-compile-exp (env-get->env _exp0) _append-preamble0)
            ".env.env)->"
            (mangle (env-get->field _exp0))))))
 
(define num-lambdas 0)
 
(define lambdas ())
 
(define allocate-lambda (lambda (lam)
      (let ((id num-lambdas))
         (set! num-lambdas (+ 1 num-lambdas))
         (set! lambdas (cons (list id lam) lambdas))
         id)))
 
(define get-lambda (<change>
      (lambda (id)
         (cdr (assv id lambdas)))
      (lambda (_id0)
         (cdr (assv _id0 lambdas)))))
 
(define c-compile-closure (lambda (exp append-preamble)
      (let* ((lam (closure->lam exp))
             (env (closure->env exp))
             (lid (allocate-lambda (c-compile-lambda lam))))
         (string-append
            "MakeClosure("
            "__lambda_"
            (number->string lid)
            ","
            (c-compile-exp env append-preamble)
            ")"))))
 
(define c-compile-formals (<change>
      (lambda (formals)
         (if (not (pair? formals))
            ""
            (string-append
               "Value "
               (mangle (car formals))
               (if (pair? (cdr formals))
                  (string-append ", " (c-compile-formals (cdr formals)))
                  ""))))
      (lambda (_formals0)
         (if (not (pair? _formals0))
            ""
            (string-append
               "Value "
               (mangle (car _formals0))
               (if (pair? (cdr _formals0))
                  (string-append ", " (c-compile-formals (cdr _formals0)))
                  ""))))))
 
(define c-compile-lambda (<change>
      (lambda (exp)
         (let* ((preamble "")
                (append-preamble (lambda (s)
                                   (set! preamble (string-append preamble "  " s "\n")))))
            (let ((formals (c-compile-formals (lambda->formals exp)))
                  (body (c-compile-exp (lambda->exp exp) append-preamble)))
               (lambda (name)
                  (string-append "Value " name "(" formals ") {\n" preamble "  return " body " ;\n" "}\n")))))
      (lambda (_exp0)
         (let* ((_preamble0 "")
                (_append-preamble0 (lambda (_s0)
                                     (set! _preamble0 (string-append _preamble0 "  " _s0 "\n")))))
            (let ((_formals0 (c-compile-formals (lambda->formals _exp0)))
                  (_body0 (c-compile-exp (lambda->exp _exp0) _append-preamble0)))
               (lambda (_name0)
                  (string-append "Value " _name0 "(" _formals0 ") {\n" _preamble0 "  return " _body0 " ;\n" "}\n")))))))
 
(define c-compile-env-struct (<change>
      (lambda (env)
         (let* ((id (car env))
                (fields (cdr env))
                (sid (number->string id))
                (tyname (string-append "struct __env_" sid)))
            (string-append
               "struct __env_"
               (number->string id)
               " {\n"
               (apply string-append (map (lambda (f) (string-append " Value " (mangle f) " ; \n")) fields))
               "} ;\n\n"
               tyname
               "*"
               " __alloc_env"
               sid
               "("
               (c-compile-formals fields)
               ")"
               "{\n"
               "  "
               tyname
               "*"
               " t = malloc(sizeof("
               tyname
               "))"
               ";\n"
               (apply
                  string-append
                  (map (lambda (f) (string-append "  t->" (mangle f) " = " (mangle f) ";\n")) fields))
               "  return t;\n"
               "}\n\n")))
      (lambda (_env0)
         (let* ((_id0 (car _env0))
                (_fields0 (cdr _env0))
                (_sid0 (number->string _id0))
                (_tyname0 (string-append "struct __env_" _sid0)))
            (string-append
               "struct __env_"
               (number->string _id0)
               " {\n"
               (apply string-append (map (lambda (_f0) (string-append " Value " (mangle _f0) " ; \n")) _fields0))
               "} ;\n\n"
               _tyname0
               "*"
               " __alloc_env"
               _sid0
               "("
               (c-compile-formals _fields0)
               ")"
               "{\n"
               "  "
               _tyname0
               "*"
               " t = malloc(sizeof("
               _tyname0
               "))"
               ";\n"
               (apply
                  string-append
                  (map (lambda (_f1) (string-append "  t->" (mangle _f1) " = " (mangle _f1) ";\n")) _fields0))
               "  return t;\n"
               "}\n\n")))))
 
(define emit (lambda (line)
      (display line)
      (newline)))
 
(define c-compile-and-emit (lambda (emit input-program)
      (define compiled-program "")
      (set! input-program (desugar input-program))
      (analyze-mutable-variables input-program)
      (set! input-program (desugar (wrap-mutables input-program)))
      (set! input-program (closure-convert input-program))
      (emit "#include <stdlib.h>")
      (emit "#include <stdio.h>")
      (emit "#include \"scheme.h\"")
      (emit "")
      (emit
         "
Value __sum ;
Value __difference ;
Value __product ;
Value __display ;
Value __numEqual ;
")
      (for-each (lambda (env) (emit (c-compile-env-struct env))) environments)
      (set! compiled-program (c-compile-program input-program))
      (emit "Value __prim_sum(Value e, Value a, Value b) {
  return MakeInt(a.z.value + b.z.value) ;
}")
      (emit
         "Value __prim_product(Value e, Value a, Value b) {
  return MakeInt(a.z.value * b.z.value) ;
}")
      (emit
         "Value __prim_difference(Value e, Value a, Value b) {
  return MakeInt(a.z.value - b.z.value) ;
}")
      (emit "Value __prim_display(Value e, Value v) {
  printf(\"%i\\n\",v.z.value) ;
  return v ;
}")
      (emit
         "Value __prim_numEqual(Value e, Value a, Value b) {
  return MakeBoolean(a.z.value == b.z.value) ;
}")
      (for-each
         (lambda (l)
            (emit (string-append "Value __lambda_" (number->string (car l)) "() ;")))
         lambdas)
      (emit "")
      (for-each
         (lambda (l)
            (emit ((cadr l) (string-append "__lambda_" (number->string (car l))))))
         lambdas)
      (emit compiled-program)))
 
(define the-program 3)
 
(c-compile-and-emit emit the-program)
 
