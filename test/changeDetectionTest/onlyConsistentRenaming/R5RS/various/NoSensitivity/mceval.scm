;; renamed lambdas/lets: 33
 
(define self-evaluating? (lambda (exp)
      @sensitivity:FA
      (if (number? exp) #t (if (string? exp) #t #f))))
 
(define variable? (lambda (exp)
      @sensitivity:FA
      (symbol? exp)))
 
(define tagged-list? (lambda (exp tag)
      @sensitivity:FA
      (if (pair? exp) (eq? (car exp) tag) #f)))
 
(define quoted? (<change>
      (lambda (exp)
         @sensitivity:FA
         (tagged-list? exp 'quote))
      (lambda (_exp0)
         @sensitivity:FA
         (tagged-list? _exp0 'quote))))
 
(define text-of-quotation (<change>
      (lambda (exp)
         @sensitivity:FA
         (cadr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (cadr _exp0))))
 
(define assignment? (lambda (exp)
      @sensitivity:FA
      (tagged-list? exp 'set!)))
 
(define assignment-variable (<change>
      (lambda (exp)
         @sensitivity:FA
         (cadr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (cadr _exp0))))
 
(define assignment-value (<change>
      (lambda (exp)
         @sensitivity:FA
         (caddr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (caddr _exp0))))
 
(define definition? (<change>
      (lambda (exp)
         @sensitivity:FA
         (tagged-list? exp 'define))
      (lambda (_exp0)
         @sensitivity:FA
         (tagged-list? _exp0 'define))))
 
(define definition-variable (lambda (exp)
      @sensitivity:FA
      (if (symbol? (cadr exp)) (cadr exp) (caadr exp))))
 
(define make-lambda (lambda (parameters body)
      @sensitivity:FA
      (cons 'lambda (cons parameters body))))
 
(define definition-value (lambda (exp)
      @sensitivity:FA
      (if (symbol? (cadr exp))
         (caddr exp)
         (make-lambda (cdadr exp) (cddr exp)))))
 
(define lambda? (<change>
      (lambda (exp)
         @sensitivity:FA
         (tagged-list? exp 'lambda))
      (lambda (_exp0)
         @sensitivity:FA
         (tagged-list? _exp0 'lambda))))
 
(define lambda-parameters (<change>
      (lambda (exp)
         @sensitivity:FA
         (cadr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (cadr _exp0))))
 
(define lambda-body (<change>
      (lambda (exp)
         @sensitivity:FA
         (cddr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (cddr _exp0))))
 
(define if? (lambda (exp)
      @sensitivity:FA
      (tagged-list? exp 'if)))
 
(define if-predicate (lambda (exp)
      @sensitivity:FA
      (cadr exp)))
 
(define if-consequent (<change>
      (lambda (exp)
         @sensitivity:FA
         (caddr exp))
      (lambda (_exp0)
         @sensitivity:FA
         (caddr _exp0))))
 
(define if-alternative (<change>
      (lambda (exp)
         @sensitivity:FA
         (if (not (null? (cdddr exp)))
            (cadddr exp)
            'false))
      (lambda (_exp0)
         @sensitivity:FA
         (if (not (null? (cdddr _exp0)))
            (cadddr _exp0)
            'false))))
 
(define make-if (lambda (predicate consequent alternative)
      @sensitivity:FA
      (cons 'if (cons predicate (cons consequent (cons alternative ()))))))
 
(define begin? (<change>
      (lambda (exp)
         @sensitivity:FA
         (tagged-list? exp 'begin))
      (lambda (_exp0)
         @sensitivity:FA
         (tagged-list? _exp0 'begin))))
 
(define begin-actions (lambda (exp)
      @sensitivity:FA
      (cdr exp)))
 
(define last-exp? (lambda (seq)
      @sensitivity:FA
      (null? (cdr seq))))
 
(define first-exp (lambda (seq)
      @sensitivity:FA
      (car seq)))
 
(define rest-exps (lambda (seq)
      @sensitivity:FA
      (cdr seq)))
 
(define mk-begin (lambda (seq)
      @sensitivity:FA
      (cons 'begin seq)))
 
(define sequence->exp (<change>
      (lambda (seq)
         @sensitivity:FA
         (if (null? seq)
            seq
            (if (last-exp? seq)
               (first-exp seq)
               (mk-begin seq))))
      (lambda (_seq0)
         @sensitivity:FA
         (if (null? _seq0)
            _seq0
            (if (last-exp? _seq0)
               (first-exp _seq0)
               (mk-begin _seq0))))))
 
(define application? (lambda (exp)
      @sensitivity:FA
      (pair? exp)))
 
(define operator (<change>
      (lambda (exp)
         @sensitivity:FA
         (car exp))
      (lambda (_exp0)
         @sensitivity:FA
         (car _exp0))))
 
(define operands (lambda (exp)
      @sensitivity:FA
      (cdr exp)))
 
(define no-operands? (<change>
      (lambda (ops)
         @sensitivity:FA
         (null? ops))
      (lambda (_ops0)
         @sensitivity:FA
         (null? _ops0))))
 
(define first-operand (<change>
      (lambda (ops)
         @sensitivity:FA
         (car ops))
      (lambda (_ops0)
         @sensitivity:FA
         (car _ops0))))
 
(define rest-operands (<change>
      (lambda (ops)
         @sensitivity:FA
         (cdr ops))
      (lambda (_ops0)
         @sensitivity:FA
         (cdr _ops0))))
 
(define cond? (<change>
      (lambda (exp)
         @sensitivity:FA
         (tagged-list? exp 'cond))
      (lambda (_exp0)
         @sensitivity:FA
         (tagged-list? _exp0 'cond))))
 
(define cond-clauses (lambda (exp)
      @sensitivity:FA
      (cdr exp)))
 
(define cond-predicate (lambda (clause)
      @sensitivity:FA
      (car clause)))
 
(define cond-else-clause? (<change>
      (lambda (clause)
         @sensitivity:FA
         (eq? (cond-predicate clause) 'else))
      (lambda (_clause0)
         @sensitivity:FA
         (eq? (cond-predicate _clause0) 'else))))
 
(define cond-actions (lambda (clause)
      @sensitivity:FA
      (cdr clause)))
 
(define expand-clauses (lambda (clauses)
      @sensitivity:No
      (if (null? clauses)
         'false
         (let ((first (car clauses))
               (rest (cdr clauses)))
            (if (cond-else-clause? first)
               (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF" clauses))
               (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))))
 
(define cond->if (lambda (exp)
      @sensitivity:FA
      (expand-clauses (cond-clauses exp))))
 
(define true? (lambda (x)
      @sensitivity:FA
      (not (eq? x #f))))
 
(define false? (<change>
      (lambda (x)
         @sensitivity:FA
         (eq? x #f))
      (lambda (_x0)
         @sensitivity:FA
         (eq? _x0 #f))))
 
(define make-procedure (lambda (parameters body env)
      @sensitivity:FA
      (cons 'procedure (cons parameters (cons body (cons env ()))))))
 
(define compound-procedure? (<change>
      (lambda (p)
         @sensitivity:FA
         (tagged-list? p 'procedure))
      (lambda (_p0)
         @sensitivity:FA
         (tagged-list? _p0 'procedure))))
 
(define procedure-parameters (lambda (p)
      @sensitivity:FA
      (cadr p)))
 
(define procedure-body (<change>
      (lambda (p)
         @sensitivity:FA
         (caddr p))
      (lambda (_p0)
         @sensitivity:FA
         (caddr _p0))))
 
(define procedure-environment (<change>
      (lambda (p)
         @sensitivity:FA
         (cadddr p))
      (lambda (_p0)
         @sensitivity:FA
         (cadddr _p0))))
 
(define enclosing-environment (lambda (env)
      @sensitivity:FA
      (cdr env)))
 
(define first-frame (<change>
      (lambda (env)
         @sensitivity:FA
         (car env))
      (lambda (_env0)
         @sensitivity:FA
         (car _env0))))
 
(define the-empty-environment ())
 
(define make-frame (<change>
      (lambda (variables values)
         @sensitivity:FA
         (cons variables values))
      (lambda (_variables0 _values0)
         @sensitivity:FA
         (cons _variables0 _values0))))
 
(define frame-variables (lambda (frame)
      @sensitivity:FA
      (car frame)))
 
(define frame-values (lambda (frame)
      @sensitivity:FA
      (cdr frame)))
 
(define add-binding-to-frame! (<change>
      (lambda (var val frame)
         @sensitivity:FA
         (set-car! frame (cons var (car frame)))
         (set-cdr! frame (cons val (cdr frame))))
      (lambda (_var0 _val0 _frame0)
         @sensitivity:FA
         (set-car! _frame0 (cons _var0 (car _frame0)))
         (set-cdr! _frame0 (cons _val0 (cdr _frame0))))))
 
(define extend-environment (lambda (vars vals base-env)
      @sensitivity:No
      (if (= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)
         (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals)))))
 
(define lookup-variable-value (<change>
      (lambda (var env)
         @sensitivity:FA
         (letrec ((env-loop (lambda (env)
                              @sensitivity:FA
                              (letrec ((scan (lambda (vars vals)
                                               @sensitivity:FA
                                               (if (null? vars)
                                                  (env-loop (enclosing-environment env))
                                                  (if (eq? var (car vars))
                                                     (car vals)
                                                     (scan (cdr vars) (cdr vals)))))))
                                 (if (eq? env the-empty-environment)
                                    (error "Unbound variable" var)
                                    (let ((frame (first-frame env)))
                                       (scan (frame-variables frame) (frame-values frame))))))))
            (env-loop env)))
      (lambda (_var0 _env0)
         @sensitivity:FA
         (letrec ((_env-loop0 (lambda (_env1)
                                @sensitivity:FA
                                (letrec ((_scan0 (lambda (_vars0 _vals0)
                                                   @sensitivity:FA
                                                   (if (null? _vars0)
                                                      (_env-loop0 (enclosing-environment _env1))
                                                      (if (eq? _var0 (car _vars0))
                                                         (car _vals0)
                                                         (_scan0 (cdr _vars0) (cdr _vals0)))))))
                                   (if (eq? _env1 the-empty-environment)
                                      (error "Unbound variable" _var0)
                                      (let ((_frame0 (first-frame _env1)))
                                         (_scan0 (frame-variables _frame0) (frame-values _frame0))))))))
            (_env-loop0 _env0)))))
 
(define set-variable-value! (lambda (var val env)
      @sensitivity:FA
      (letrec ((env-loop (lambda (env)
                           @sensitivity:FA
                           (<change>
                              (letrec ((scan (lambda (vars vals)
                                               @sensitivity:FA
                                               (if (null? vars)
                                                  (env-loop (enclosing-environment env))
                                                  (if (eq? var (car vars))
                                                     (set-car! vals val)
                                                     (scan (cdr vars) (cdr vals)))))))
                                 (if (eq? env the-empty-environment)
                                    (error "Unbound variable -- SET!" var)
                                    (let ((frame (first-frame env)))
                                       (scan (frame-variables frame) (frame-values frame)))))
                              (letrec ((_scan0 (lambda (_vars0 _vals0)
                                                 @sensitivity:FA
                                                 (if (null? _vars0)
                                                    (env-loop (enclosing-environment env))
                                                    (if (eq? var (car _vars0))
                                                       (set-car! _vals0 val)
                                                       (_scan0 (cdr _vars0) (cdr _vals0)))))))
                                 (if (eq? env the-empty-environment)
                                    (error "Unbound variable -- SET!" var)
                                    (let ((_frame0 (first-frame env)))
                                       (_scan0 (frame-variables _frame0) (frame-values _frame0)))))))))
         (env-loop env))))
 
(define define-variable! (lambda (var val env)
      @sensitivity:FA
      (<change>
         (let ((frame (first-frame env)))
            (letrec ((scan (lambda (vars vals)
                             @sensitivity:FA
                             (if (null? vars)
                                (add-binding-to-frame! var val frame)
                                (if (eq? var (car vars))
                                   (set-car! vals val)
                                   (scan (cdr vars) (cdr vals)))))))
               (scan (frame-variables frame) (frame-values frame))))
         (let ((_frame0 (first-frame env)))
            (letrec ((_scan0 (lambda (_vars0 _vals0)
                               @sensitivity:FA
                               (if (null? _vars0)
                                  (add-binding-to-frame! var val _frame0)
                                  (if (eq? var (car _vars0))
                                     (set-car! _vals0 val)
                                     (_scan0 (cdr _vars0) (cdr _vals0)))))))
               (_scan0 (frame-variables _frame0) (frame-values _frame0)))))))
 
(define primitive-procedure? (lambda (proc)
      @sensitivity:FA
      (tagged-list? proc 'primitive)))
 
(define primitive-implementation (<change>
      (lambda (proc)
         @sensitivity:FA
         (cadr proc))
      (lambda (_proc0)
         @sensitivity:FA
         (cadr _proc0))))
 
(define primitive-procedures (cons (cons '= (cons = ())) (cons (cons '* (cons * ())) (cons (cons '- (cons - ())) ()))))
 
(define primitive-procedure-names (<change>
      (lambda ()
         @sensitivity:FA
         (map car primitive-procedures))
      (lambda ()
         @sensitivity:FA
         (map car primitive-procedures))))
 
(define primitive-procedure-objects (<change>
      (lambda ()
         @sensitivity:FA
         (map (lambda (proc) (cons 'primitive (cons (cadr proc) ()))) primitive-procedures))
      (lambda ()
         @sensitivity:FA
         (map (lambda (_proc0) (cons 'primitive (cons (cadr _proc0) ()))) primitive-procedures))))
 
(define setup-environment (lambda ()
      (let ((initial-env (extend-environment
                           (primitive-procedure-names)
                           (primitive-procedure-objects)
                           the-empty-environment)))
         (define-variable! 'true #t initial-env)
         (define-variable! 'false #f initial-env)
         initial-env)))
 
(define the-global-environment (setup-environment))
 
(define apply-primitive-procedure (lambda (proc args)
      @sensitivity:FA
      (<change>
         (let ((f (primitive-implementation proc))
               (n (length args)))
            (if (= n 0)
               (f)
               (if (= n 1)
                  (f (car args))
                  (if (= n 2)
                     (f (car args) (cadr args))
                     (error "ERROR -- can't handle more than two arguments")))))
         (let ((_f0 (primitive-implementation proc))
               (_n0 (length args)))
            (if (= _n0 0)
               (_f0)
               (if (= _n0 1)
                  (_f0 (car args))
                  (if (= _n0 2)
                     (_f0 (car args) (cadr args))
                     (error "ERROR -- can't handle more than two arguments"))))))))
 
(define mceval (<change>
      (lambda (exp env)
         @sensitivity:FA
         (letrec ((eval-sequence (lambda (exps env)
                                   @sensitivity:FA
                                   (if (last-exp? exps)
                                      (mceval (first-exp exps) env)
                                      (begin
                                         (mceval (first-exp exps) env)
                                         (eval-sequence (rest-exps exps) env))))))
            (let ((mcapply (lambda (procedure arguments)
                             @sensitivity:FA
                             (if (primitive-procedure? procedure)
                                (apply-primitive-procedure procedure arguments)
                                (if (compound-procedure? procedure)
                                   (eval-sequence
                                      (procedure-body procedure)
                                      (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure)))
                                   (error "Unknown procedure type -- APPLY" procedure))))))
               (let ((eval-if (lambda (exp env)
                                @sensitivity:FA
                                (if (true? (mceval (if-predicate exp) env))
                                   (mceval (if-consequent exp) env)
                                   (mceval (if-alternative exp) env)))))
                  (let ((eval-assignment (lambda (exp env)
                                           (set-variable-value! (assignment-variable exp) (mceval (assignment-value exp) env) env)
                                           'ok)))
                     (let ((eval-definition (lambda (exp env)
                                              @sensitivity:FA
                                              (define-variable! (definition-variable exp) (mceval (definition-value exp) env) env)
                                              'ok)))
                        (letrec ((list-of-values (lambda (exps env)
                                                   @sensitivity:FA
                                                   (if (no-operands? exps)
                                                      ()
                                                      (cons (mceval (first-operand exps) env) (list-of-values (rest-operands exps) env))))))
                           (if (self-evaluating? exp)
                              exp
                              (if (variable? exp)
                                 (lookup-variable-value exp env)
                                 (if (quoted? exp)
                                    (text-of-quotation exp)
                                    (if (assignment? exp)
                                       (eval-assignment exp env)
                                       (if (definition? exp)
                                          (eval-definition exp env)
                                          (if (if? exp)
                                             (eval-if exp env)
                                             (if (lambda? exp)
                                                (make-procedure (lambda-parameters exp) (lambda-body exp) env)
                                                (if (begin? exp)
                                                   (eval-sequence (begin-actions exp) env)
                                                   (if (cond? exp)
                                                      (mceval (cond->if exp) env)
                                                      (if (application? exp)
                                                         (mcapply (mceval (operator exp) env) (list-of-values (operands exp) env))
                                                         (error "Unknown expression type -- EVAL" exp))))))))))))))))))
      (lambda (_exp0 _env0)
         @sensitivity:FA
         (letrec ((_eval-sequence0 (lambda (_exps0 _env1)
                                     @sensitivity:FA
                                     (if (last-exp? _exps0)
                                        (mceval (first-exp _exps0) _env1)
                                        (begin
                                           (mceval (first-exp _exps0) _env1)
                                           (_eval-sequence0 (rest-exps _exps0) _env1))))))
            (let ((_mcapply0 (lambda (_procedure0 _arguments0)
                               @sensitivity:FA
                               (if (primitive-procedure? _procedure0)
                                  (apply-primitive-procedure _procedure0 _arguments0)
                                  (if (compound-procedure? _procedure0)
                                     (_eval-sequence0
                                        (procedure-body _procedure0)
                                        (extend-environment
                                           (procedure-parameters _procedure0)
                                           _arguments0
                                           (procedure-environment _procedure0)))
                                     (error "Unknown procedure type -- APPLY" _procedure0))))))
               (let ((_eval-if0 (lambda (_exp1 _env2)
                                  @sensitivity:FA
                                  (if (true? (mceval (if-predicate _exp1) _env2))
                                     (mceval (if-consequent _exp1) _env2)
                                     (mceval (if-alternative _exp1) _env2)))))
                  (let ((_eval-assignment0 (lambda (_exp2 _env3)
                                             (set-variable-value! (assignment-variable _exp2) (mceval (assignment-value _exp2) _env3) _env3)
                                             'ok)))
                     (let ((_eval-definition0 (lambda (_exp3 _env4)
                                                @sensitivity:FA
                                                (define-variable! (definition-variable _exp3) (mceval (definition-value _exp3) _env4) _env4)
                                                'ok)))
                        (letrec ((_list-of-values0 (lambda (_exps1 _env5)
                                                     @sensitivity:FA
                                                     (if (no-operands? _exps1)
                                                        ()
                                                        (cons (mceval (first-operand _exps1) _env5) (_list-of-values0 (rest-operands _exps1) _env5))))))
                           (if (self-evaluating? _exp0)
                              _exp0
                              (if (variable? _exp0)
                                 (lookup-variable-value _exp0 _env0)
                                 (if (quoted? _exp0)
                                    (text-of-quotation _exp0)
                                    (if (assignment? _exp0)
                                       (_eval-assignment0 _exp0 _env0)
                                       (if (definition? _exp0)
                                          (_eval-definition0 _exp0 _env0)
                                          (if (if? _exp0)
                                             (_eval-if0 _exp0 _env0)
                                             (if (lambda? _exp0)
                                                (make-procedure (lambda-parameters _exp0) (lambda-body _exp0) _env0)
                                                (if (begin? _exp0)
                                                   (_eval-sequence0 (begin-actions _exp0) _env0)
                                                   (if (cond? _exp0)
                                                      (mceval (cond->if _exp0) _env0)
                                                      (if (application? _exp0)
                                                         (_mcapply0 (mceval (operator _exp0) _env0) (_list-of-values0 (operands _exp0) _env0))
                                                         (error "Unknown expression type -- EVAL" _exp0))))))))))))))))))))
 
(mceval
   (__toplevel_cons
      (__toplevel_cons
         (__toplevel_cons
            'lambda
            (__toplevel_cons
               (__toplevel_cons 'f ())
               (__toplevel_cons
                  (__toplevel_cons
                     'lambda
                     (__toplevel_cons
                        (__toplevel_cons 'x ())
                        (__toplevel_cons (__toplevel_cons 'f (__toplevel_cons 'f (__toplevel_cons 'x ()))) ())))
                  ())))
         (__toplevel_cons
            (__toplevel_cons
               'lambda
               (__toplevel_cons
                  (__toplevel_cons 'g (__toplevel_cons 'n ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        'if
                        (__toplevel_cons
                           (__toplevel_cons '= (__toplevel_cons 'n (__toplevel_cons 0 ())))
                           (__toplevel_cons
                              1
                              (__toplevel_cons
                                 (__toplevel_cons
                                    '*
                                    (__toplevel_cons
                                       'n
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'g
                                             (__toplevel_cons
                                                'g
                                                (__toplevel_cons (__toplevel_cons '- (__toplevel_cons 'n (__toplevel_cons 1 ()))) ())))
                                          ())))
                                 ()))))
                     ())))
            ()))
      (__toplevel_cons 8 ()))
   the-global-environment)
 
