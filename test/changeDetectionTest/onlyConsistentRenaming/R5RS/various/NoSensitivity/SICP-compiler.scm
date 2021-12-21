;; renamed lambdas/lets: 43
 
(define true #t)
 
(define false #f)
 
(define self-evaluating? (<change>
      (lambda (exp)
         (if (number? exp)
            true
            (if (string? exp) true false)))
      (lambda (_exp0)
         (if (number? _exp0)
            true
            (if (string? _exp0) true false)))))
 
(define quoted? (<change>
      (lambda (exp)
         (tagged-list? exp 'quote))
      (lambda (_exp0)
         (tagged-list? _exp0 'quote))))
 
(define text-of-quotation (lambda (exp)
      (cadr exp)))
 
(define tagged-list? (lambda (exp tag)
      (if (pair? exp) (eq? (car exp) tag) false)))
 
(define variable? (<change>
      (lambda (exp)
         (symbol? exp))
      (lambda (_exp0)
         (symbol? _exp0))))
 
(define assignment? (lambda (exp)
      (tagged-list? exp 'set!)))
 
(define assignment-variable (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define assignment-value (<change>
      (lambda (exp)
         (caddr exp))
      (lambda (_exp0)
         (caddr _exp0))))
 
(define definition? (<change>
      (lambda (exp)
         (tagged-list? exp 'define))
      (lambda (_exp0)
         (tagged-list? _exp0 'define))))
 
(define definition-variable (<change>
      (lambda (exp)
         (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
      (lambda (_exp0)
         (if (symbol? (cadr _exp0))
            (cadr _exp0)
            (caadr _exp0)))))
 
(define definition-value (lambda (exp)
      (if (symbol? (cadr exp))
         (caddr exp)
         (make-lambda (cdadr exp) (cddr exp)))))
 
(define lambda? (<change>
      (lambda (exp)
         (tagged-list? exp 'lambda))
      (lambda (_exp0)
         (tagged-list? _exp0 'lambda))))
 
(define lambda-parameters (<change>
      (lambda (exp)
         (cadr exp))
      (lambda (_exp0)
         (cadr _exp0))))
 
(define lambda-body (<change>
      (lambda (exp)
         (cddr exp))
      (lambda (_exp0)
         (cddr _exp0))))
 
(define make-lambda (lambda (parameters body)
      (cons 'lambda (cons parameters body))))
 
(define if? (<change>
      (lambda (exp)
         (tagged-list? exp 'if))
      (lambda (_exp0)
         (tagged-list? _exp0 'if))))
 
(define if-predicate (lambda (exp)
      (cadr exp)))
 
(define if-consequent (lambda (exp)
      (caddr exp)))
 
(define if-alternative (<change>
      (lambda (exp)
         (if (not (null? (cdddr exp)))
            (cadddr exp)
            'false))
      (lambda (_exp0)
         (if (not (null? (cdddr _exp0)))
            (cadddr _exp0)
            'false))))
 
(define begin? (lambda (exp)
      (tagged-list? exp 'begin)))
 
(define begin-actions (lambda (exp)
      (cdr exp)))
 
(define last-exp? (<change>
      (lambda (seq)
         (null? (cdr seq)))
      (lambda (_seq0)
         (null? (cdr _seq0)))))
 
(define first-exp (<change>
      (lambda (seq)
         (car seq))
      (lambda (_seq0)
         (car _seq0))))
 
(define rest-exps (lambda (seq)
      (cdr seq)))
 
(define application? (<change>
      (lambda (exp)
         (pair? exp))
      (lambda (_exp0)
         (pair? _exp0))))
 
(define operator (<change>
      (lambda (exp)
         (car exp))
      (lambda (_exp0)
         (car _exp0))))
 
(define operands (lambda (exp)
      (cdr exp)))
 
(define no-operands? (<change>
      (lambda (ops)
         (null? ops))
      (lambda (_ops0)
         (null? _ops0))))
 
(define first-operand (<change>
      (lambda (ops)
         (car ops))
      (lambda (_ops0)
         (car _ops0))))
 
(define rest-operands (<change>
      (lambda (ops)
         (cdr ops))
      (lambda (_ops0)
         (cdr _ops0))))
 
(define make-if (lambda (predicate consequent alternative)
      (cons 'if (cons predicate (cons consequent (cons alternative ()))))))
 
(define sequence->exp (lambda (seq)
      (if (null? seq)
         seq
         (if (last-exp? seq)
            (first-exp seq)
            (make-begin seq)))))
 
(define make-begin (<change>
      (lambda (seq)
         (cons 'begin seq))
      (lambda (_seq0)
         (cons 'begin _seq0))))
 
(define cond? (<change>
      (lambda (exp)
         (tagged-list? exp 'cond))
      (lambda (_exp0)
         (tagged-list? _exp0 'cond))))
 
(define cond-clauses (<change>
      (lambda (exp)
         (cdr exp))
      (lambda (_exp0)
         (cdr _exp0))))
 
(define cond-else-clause? (<change>
      (lambda (clause)
         (eq? (cond-predicate clause) 'else))
      (lambda (_clause0)
         (eq? (cond-predicate _clause0) 'else))))
 
(define cond-predicate (<change>
      (lambda (clause)
         (car clause))
      (lambda (_clause0)
         (car _clause0))))
 
(define cond-actions (lambda (clause)
      (cdr clause)))
 
(define cond->if (lambda (exp)
      (expand-clauses (cond-clauses exp))))
 
(define expand-clauses (<change>
      (lambda (clauses)
         (if (null? clauses)
            'false
            (let ((first (car clauses))
                  (rest (cdr clauses)))
               (if (cond-else-clause? first)
                  (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last -- COND->IF" clauses))
                  (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest))))))
      (lambda (_clauses0)
         (if (null? _clauses0)
            'false
            (let ((_first0 (car _clauses0))
                  (_rest0 (cdr _clauses0)))
               (if (cond-else-clause? _first0)
                  (if (null? _rest0)
                     (sequence->exp (cond-actions _first0))
                     (error "ELSE clause isn't last -- COND->IF" _clauses0))
                  (make-if (cond-predicate _first0) (sequence->exp (cond-actions _first0)) (expand-clauses _rest0))))))))
 
(define compile (lambda (exp target linkage)
      (if (self-evaluating? exp)
         (compile-self-evaluating exp target linkage)
         (if (quoted? exp)
            (compile-quoted exp target linkage)
            (if (variable? exp)
               (compile-variable exp target linkage)
               (if (assignment? exp)
                  (compile-assignment exp target linkage)
                  (if (definition? exp)
                     (compile-definition exp target linkage)
                     (if (if? exp)
                        (compile-if exp target linkage)
                        (if (lambda? exp)
                           (compile-lambda exp target linkage)
                           (if (begin? exp)
                              (compile-sequence (begin-actions exp) target linkage)
                              (if (cond? exp)
                                 (compile (cond->if exp) target linkage)
                                 (if (application? exp)
                                    (compile-application exp target linkage)
                                    (error "Unknown expression type -- COMPILE" exp)))))))))))))
 
(define make-instruction-sequence (lambda (needs modifies statements)
      (cons needs (cons modifies (cons statements ())))))
 
(define empty-instruction-sequence (lambda ()
      (make-instruction-sequence () () ())))
 
(define compile-linkage (lambda (linkage)
      (if (eq? linkage 'return)
         (make-instruction-sequence
            (__toplevel_cons 'continue ())
            ()
            (__toplevel_cons
               (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
               ()))
         (if (eq? linkage 'next)
            (empty-instruction-sequence)
            (make-instruction-sequence
               ()
               ()
               (__toplevel_cons
                  (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ()))
                  ()))))))
 
(define end-with-linkage (lambda (linkage instruction-sequence)
      (preserving (__toplevel_cons 'continue ()) instruction-sequence (compile-linkage linkage))))
 
(define compile-self-evaluating (<change>
      (lambda (exp target linkage)
         (end-with-linkage
            linkage
            (make-instruction-sequence
               ()
               (cons target ())
               (__toplevel_cons
                  (__toplevel_cons
                     'assign
                     (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons exp ())) ())))
                  ()))))
      (lambda (_exp0 _target0 _linkage0)
         (end-with-linkage
            _linkage0
            (make-instruction-sequence
               ()
               (cons _target0 ())
               (__toplevel_cons
                  (__toplevel_cons
                     'assign
                     (__toplevel_cons _target0 (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons _exp0 ())) ())))
                  ()))))))
 
(define compile-quoted (lambda (exp target linkage)
      (end-with-linkage
         linkage
         (make-instruction-sequence
            ()
            (cons target ())
            (__toplevel_cons
               (__toplevel_cons
                  'assign
                  (__toplevel_cons
                     target
                     (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons (text-of-quotation exp) ())) ())))
               ())))))
 
(define compile-variable (lambda (exp target linkage)
      (end-with-linkage
         linkage
         (make-instruction-sequence
            (__toplevel_cons 'env ())
            (cons target ())
            (__toplevel_cons
               (__toplevel_cons
                  'assign
                  (__toplevel_cons
                     target
                     (__toplevel_cons
                        (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                        (__toplevel_cons
                           (__toplevel_cons 'const (__toplevel_cons exp ()))
                           (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
               ())))))
 
(define compile-assignment (lambda (exp target linkage)
      (let ((var (assignment-variable exp))
            (get-value-code (compile (assignment-value exp) 'val 'next)))
         (end-with-linkage
            linkage
            (preserving
               (__toplevel_cons 'env ())
               get-value-code
               (make-instruction-sequence
                  (__toplevel_cons 'env (__toplevel_cons 'val ()))
                  (cons target ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'perform
                        (__toplevel_cons
                           (__toplevel_cons 'op (__toplevel_cons 'set-variable-value! ()))
                           (__toplevel_cons
                              (__toplevel_cons 'const (__toplevel_cons var ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           'assign
                           (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                        ()))))))))
 
(define compile-definition (lambda (exp target linkage)
      (<change>
         (let ((var (definition-variable exp))
               (get-value-code (compile (definition-value exp) 'val 'next)))
            (end-with-linkage
               linkage
               (preserving
                  (__toplevel_cons 'env ())
                  get-value-code
                  (make-instruction-sequence
                     (__toplevel_cons 'env (__toplevel_cons 'val ()))
                     (cons target ())
                     (__toplevel_cons
                        (__toplevel_cons
                           'perform
                           (__toplevel_cons
                              (__toplevel_cons 'op (__toplevel_cons 'define-variable! ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'const (__toplevel_cons var ()))
                                 (__toplevel_cons
                                    (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                           ()))))))
         (let ((_var0 (definition-variable exp))
               (_get-value-code0 (compile (definition-value exp) 'val 'next)))
            (end-with-linkage
               linkage
               (preserving
                  (__toplevel_cons 'env ())
                  _get-value-code0
                  (make-instruction-sequence
                     (__toplevel_cons 'env (__toplevel_cons 'val ()))
                     (cons target ())
                     (__toplevel_cons
                        (__toplevel_cons
                           'perform
                           (__toplevel_cons
                              (__toplevel_cons 'op (__toplevel_cons 'define-variable! ()))
                              (__toplevel_cons
                                 (__toplevel_cons 'const (__toplevel_cons _var0 ()))
                                 (__toplevel_cons
                                    (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                           ())))))))))
 
(define label-counter 0)
 
(define new-label-number (<change>
      (lambda ()
         (set! label-counter (+ 1 label-counter))
         label-counter)
      (lambda ()
         (set! label-counter (+ 1 label-counter))
         label-counter)))
 
(define make-label (lambda (name)
      (string->symbol (string-append (symbol->string name) (number->string (new-label-number))))))
 
(define compile-if (<change>
      (lambda (exp target linkage)
         (let ((t-branch (make-label 'true-branch))
               (f-branch (make-label 'false-branch))
               (after-if (make-label 'after-if)))
            (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
               (let ((p-code (compile (if-predicate exp) 'val 'next))
                     (c-code (compile (if-consequent exp) target consequent-linkage))
                     (a-code (compile (if-alternative exp) target linkage)))
                  (preserving
                     (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                     p-code
                     (append-instruction-sequences
                        (cons
                           (make-instruction-sequence
                              (__toplevel_cons 'val ())
                              ()
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'test
                                    (__toplevel_cons
                                       (__toplevel_cons 'op (__toplevel_cons 'false? ()))
                                       (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'branch
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons f-branch ())) ()))
                                    ())))
                           (cons
                              (parallel-instruction-sequences
                                 (append-instruction-sequences t-branch c-code)
                                 (append-instruction-sequences f-branch a-code))
                              (cons after-if ())))))))))
      (lambda (_exp0 _target0 _linkage0)
         (let ((_t-branch0 (make-label 'true-branch))
               (_f-branch0 (make-label 'false-branch))
               (_after-if0 (make-label 'after-if)))
            (let ((_consequent-linkage0 (if (eq? _linkage0 'next) _after-if0 _linkage0)))
               (let ((_p-code0 (compile (if-predicate _exp0) 'val 'next))
                     (_c-code0 (compile (if-consequent _exp0) _target0 _consequent-linkage0))
                     (_a-code0 (compile (if-alternative _exp0) _target0 _linkage0)))
                  (preserving
                     (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                     _p-code0
                     (append-instruction-sequences
                        (cons
                           (make-instruction-sequence
                              (__toplevel_cons 'val ())
                              ()
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'test
                                    (__toplevel_cons
                                       (__toplevel_cons 'op (__toplevel_cons 'false? ()))
                                       (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'branch
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons _f-branch0 ())) ()))
                                    ())))
                           (cons
                              (parallel-instruction-sequences
                                 (append-instruction-sequences _t-branch0 _c-code0)
                                 (append-instruction-sequences _f-branch0 _a-code0))
                              (cons _after-if0 ())))))))))))
 
(define compile-sequence (<change>
      (lambda (seq target linkage)
         (if (last-exp? seq)
            (compile (first-exp seq) target linkage)
            (preserving
               (__toplevel_cons 'env (__toplevel_cons 'continue ()))
               (compile (first-exp seq) target 'next)
               (compile-sequence (rest-exps seq) target linkage))))
      (lambda (_seq0 _target0 _linkage0)
         (if (last-exp? _seq0)
            (compile (first-exp _seq0) _target0 _linkage0)
            (preserving
               (__toplevel_cons 'env (__toplevel_cons 'continue ()))
               (compile (first-exp _seq0) _target0 'next)
               (compile-sequence (rest-exps _seq0) _target0 _linkage0))))))
 
(define compile-lambda (<change>
      (lambda (exp target linkage)
         (let ((proc-entry (make-label 'entry))
               (after-lambda (make-label 'after-lambda)))
            (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
               (append-instruction-sequences
                  (cons
                     (tack-on-instruction-sequence
                        (end-with-linkage
                           lambda-linkage
                           (make-instruction-sequence
                              (__toplevel_cons 'env ())
                              (cons target ())
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       target
                                       (__toplevel_cons
                                          (__toplevel_cons 'op (__toplevel_cons 'make-compiled-procedure ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'label (__toplevel_cons proc-entry ()))
                                             (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                 ())))
                        (compile-lambda-body exp proc-entry))
                     (cons after-lambda ()))))))
      (lambda (_exp0 _target0 _linkage0)
         (let ((_proc-entry0 (make-label 'entry))
               (_after-lambda0 (make-label 'after-lambda)))
            (let ((_lambda-linkage0 (if (eq? _linkage0 'next)
                                      _after-lambda0
                                      _linkage0)))
               (append-instruction-sequences
                  (cons
                     (tack-on-instruction-sequence
                        (end-with-linkage
                           _lambda-linkage0
                           (make-instruction-sequence
                              (__toplevel_cons 'env ())
                              (cons _target0 ())
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       _target0
                                       (__toplevel_cons
                                          (__toplevel_cons 'op (__toplevel_cons 'make-compiled-procedure ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'label (__toplevel_cons _proc-entry0 ()))
                                             (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                 ())))
                        (compile-lambda-body _exp0 _proc-entry0))
                     (cons _after-lambda0 ()))))))))
 
(define compile-lambda-body (<change>
      (lambda (exp proc-entry)
         (let ((formals (lambda-parameters exp)))
            (append-instruction-sequences
               (cons
                  (make-instruction-sequence
                     (__toplevel_cons 'env (__toplevel_cons 'proc (__toplevel_cons 'argl ())))
                     (__toplevel_cons 'env ())
                     (__toplevel_cons
                        proc-entry
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons
                                 'env
                                 (__toplevel_cons
                                    (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-env ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'assign
                                 (__toplevel_cons
                                    'env
                                    (__toplevel_cons
                                       (__toplevel_cons 'op (__toplevel_cons 'extend-environment ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'const (__toplevel_cons formals ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                             (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                              ()))))
                  (cons (compile-sequence (lambda-body exp) 'val 'return) ())))))
      (lambda (_exp0 _proc-entry0)
         (let ((_formals0 (lambda-parameters _exp0)))
            (append-instruction-sequences
               (cons
                  (make-instruction-sequence
                     (__toplevel_cons 'env (__toplevel_cons 'proc (__toplevel_cons 'argl ())))
                     (__toplevel_cons 'env ())
                     (__toplevel_cons
                        _proc-entry0
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons
                                 'env
                                 (__toplevel_cons
                                    (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-env ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'assign
                                 (__toplevel_cons
                                    'env
                                    (__toplevel_cons
                                       (__toplevel_cons 'op (__toplevel_cons 'extend-environment ()))
                                       (__toplevel_cons
                                          (__toplevel_cons 'const (__toplevel_cons _formals0 ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                             (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                              ()))))
                  (cons (compile-sequence (lambda-body _exp0) 'val 'return) ())))))))
 
(define compile-application (<change>
      (lambda (exp target linkage)
         (let ((proc-code (compile (operator exp) 'proc 'next))
               (operand-codes (map (lambda (operand) (compile operand 'val 'next)) (operands exp))))
            (preserving
               (__toplevel_cons 'env (__toplevel_cons 'continue ()))
               proc-code
               (preserving
                  (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                  (construct-arglist operand-codes)
                  (compile-procedure-call target linkage)))))
      (lambda (_exp0 _target0 _linkage0)
         (let ((_proc-code0 (compile (operator _exp0) 'proc 'next))
               (_operand-codes0 (map (lambda (_operand0) (compile _operand0 'val 'next)) (operands _exp0))))
            (preserving
               (__toplevel_cons 'env (__toplevel_cons 'continue ()))
               _proc-code0
               (preserving
                  (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                  (construct-arglist _operand-codes0)
                  (compile-procedure-call _target0 _linkage0)))))))
 
(define construct-arglist (<change>
      (lambda (operand-codes)
         (let ((operand-codes (reverse operand-codes)))
            (if (null? operand-codes)
               (make-instruction-sequence
                  ()
                  (__toplevel_cons 'argl ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'assign
                        (__toplevel_cons 'argl (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons () ())) ())))
                     ()))
               (let ((code-to-get-last-arg (append-instruction-sequences
                                             (cons
                                                (car operand-codes)
                                                (cons
                                                   (make-instruction-sequence
                                                      (__toplevel_cons 'val ())
                                                      (__toplevel_cons 'argl ())
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'assign
                                                            (__toplevel_cons
                                                               'argl
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'op (__toplevel_cons 'list ()))
                                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))))
                                                         ()))
                                                   ())))))
                  (if (null? (cdr operand-codes))
                     code-to-get-last-arg
                     (preserving
                        (__toplevel_cons 'env ())
                        code-to-get-last-arg
                        (code-to-get-rest-args (cdr operand-codes))))))))
      (lambda (_operand-codes0)
         (let ((_operand-codes1 (reverse _operand-codes0)))
            (if (null? _operand-codes1)
               (make-instruction-sequence
                  ()
                  (__toplevel_cons 'argl ())
                  (__toplevel_cons
                     (__toplevel_cons
                        'assign
                        (__toplevel_cons 'argl (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons () ())) ())))
                     ()))
               (let ((_code-to-get-last-arg0 (append-instruction-sequences
                                               (cons
                                                  (car _operand-codes1)
                                                  (cons
                                                     (make-instruction-sequence
                                                        (__toplevel_cons 'val ())
                                                        (__toplevel_cons 'argl ())
                                                        (__toplevel_cons
                                                           (__toplevel_cons
                                                              'assign
                                                              (__toplevel_cons
                                                                 'argl
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'op (__toplevel_cons 'list ()))
                                                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))))
                                                           ()))
                                                     ())))))
                  (if (null? (cdr _operand-codes1))
                     _code-to-get-last-arg0
                     (preserving
                        (__toplevel_cons 'env ())
                        _code-to-get-last-arg0
                        (code-to-get-rest-args (cdr _operand-codes1))))))))))
 
(define code-to-get-rest-args (lambda (operand-codes)
      (<change>
         (let ((code-for-next-arg (preserving
                                    (__toplevel_cons 'argl ())
                                    (car operand-codes)
                                    (make-instruction-sequence
                                       (__toplevel_cons 'val (__toplevel_cons 'argl ()))
                                       (__toplevel_cons 'argl ())
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'assign
                                             (__toplevel_cons
                                                'argl
                                                (__toplevel_cons
                                                   (__toplevel_cons 'op (__toplevel_cons 'cons ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                      (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                          ())))))
            (if (null? (cdr operand-codes))
               code-for-next-arg
               (preserving
                  (__toplevel_cons 'env ())
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes)))))
         (let ((_code-for-next-arg0 (preserving
                                      (__toplevel_cons 'argl ())
                                      (car operand-codes)
                                      (make-instruction-sequence
                                         (__toplevel_cons 'val (__toplevel_cons 'argl ()))
                                         (__toplevel_cons 'argl ())
                                         (__toplevel_cons
                                            (__toplevel_cons
                                               'assign
                                               (__toplevel_cons
                                                  'argl
                                                  (__toplevel_cons
                                                     (__toplevel_cons 'op (__toplevel_cons 'cons ()))
                                                     (__toplevel_cons
                                                        (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                        (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                            ())))))
            (if (null? (cdr operand-codes))
               _code-for-next-arg0
               (preserving
                  (__toplevel_cons 'env ())
                  _code-for-next-arg0
                  (code-to-get-rest-args (cdr operand-codes))))))))
 
(define compile-procedure-call (lambda (target linkage)
      (<change>
         (let ((primitive-branch (make-label 'primitive-branch))
               (compiled-branch (make-label 'compiled-branch))
               (after-call (make-label 'after-call)))
            (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
               (append-instruction-sequences
                  (cons
                     (make-instruction-sequence
                        (__toplevel_cons 'proc ())
                        ()
                        (__toplevel_cons
                           (__toplevel_cons
                              'test
                              (__toplevel_cons
                                 (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'branch
                                 (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons primitive-branch ())) ()))
                              ())))
                     (cons
                        (parallel-instruction-sequences
                           (append-instruction-sequences
                              (cons compiled-branch (cons (compile-proc-appl target compiled-linkage) ())))
                           (append-instruction-sequences
                              (cons
                                 primitive-branch
                                 (cons
                                    (end-with-linkage
                                       linkage
                                       (make-instruction-sequence
                                          (__toplevel_cons 'proc (__toplevel_cons 'argl ()))
                                          (cons target ())
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'assign
                                                (__toplevel_cons
                                                   target
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                         (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                             ())))
                                    ()))))
                        (cons after-call ()))))))
         (let ((_primitive-branch0 (make-label 'primitive-branch))
               (_compiled-branch0 (make-label 'compiled-branch))
               (_after-call0 (make-label 'after-call)))
            (let ((_compiled-linkage0 (if (eq? linkage 'next) _after-call0 linkage)))
               (append-instruction-sequences
                  (cons
                     (make-instruction-sequence
                        (__toplevel_cons 'proc ())
                        ()
                        (__toplevel_cons
                           (__toplevel_cons
                              'test
                              (__toplevel_cons
                                 (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'branch
                                 (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons _primitive-branch0 ())) ()))
                              ())))
                     (cons
                        (parallel-instruction-sequences
                           (append-instruction-sequences
                              (cons _compiled-branch0 (cons (compile-proc-appl target _compiled-linkage0) ())))
                           (append-instruction-sequences
                              (cons
                                 _primitive-branch0
                                 (cons
                                    (end-with-linkage
                                       linkage
                                       (make-instruction-sequence
                                          (__toplevel_cons 'proc (__toplevel_cons 'argl ()))
                                          (cons target ())
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'assign
                                                (__toplevel_cons
                                                   target
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                         (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                             ())))
                                    ()))))
                        (cons _after-call0 ())))))))))
 
(define compile-proc-appl (<change>
      (lambda (target linkage)
         (if (if (eq? target 'val) (not (eq? linkage 'return)) #f)
            (make-instruction-sequence
               (__toplevel_cons 'proc ())
               all-regs
               (__toplevel_cons
                  (__toplevel_cons
                     'assign
                     (__toplevel_cons
                        'continue
                        (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'assign
                        (__toplevel_cons
                           'val
                           (__toplevel_cons
                              (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                     (__toplevel_cons
                        (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                        ()))))
            (if (if (not (eq? target 'val)) (not (eq? linkage 'return)) #f)
               (let ((proc-return (make-label 'proc-return)))
                  (make-instruction-sequence
                     (__toplevel_cons 'proc ())
                     all-regs
                     (__toplevel_cons
                        (__toplevel_cons
                           'assign
                           (__toplevel_cons
                              'continue
                              (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons proc-return ())) ())))
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons
                                 'val
                                 (__toplevel_cons
                                    (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                           (__toplevel_cons
                              (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                              (__toplevel_cons
                                 proc-return
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'assign
                                       (__toplevel_cons target (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                    (__toplevel_cons
                                       (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons linkage ())) ()))
                                       ()))))))))
               (if (if (eq? target 'val) (eq? linkage 'return) #f)
                  (make-instruction-sequence
                     (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                     all-regs
                     (__toplevel_cons
                        (__toplevel_cons
                           'assign
                           (__toplevel_cons
                              'val
                              (__toplevel_cons
                                 (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                        (__toplevel_cons
                           (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                           ())))
                  (if (if (not (eq? target 'val)) (eq? linkage 'return) #f)
                     (error "return linkage, target not val -- COMPILE" target)
                     #f)))))
      (lambda (_target0 _linkage0)
         (if (if (eq? _target0 'val) (not (eq? _linkage0 'return)) #f)
            (make-instruction-sequence
               (__toplevel_cons 'proc ())
               all-regs
               (__toplevel_cons
                  (__toplevel_cons
                     'assign
                     (__toplevel_cons
                        'continue
                        (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons _linkage0 ())) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'assign
                        (__toplevel_cons
                           'val
                           (__toplevel_cons
                              (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                     (__toplevel_cons
                        (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                        ()))))
            (if (if (not (eq? _target0 'val)) (not (eq? _linkage0 'return)) #f)
               (let ((_proc-return0 (make-label 'proc-return)))
                  (make-instruction-sequence
                     (__toplevel_cons 'proc ())
                     all-regs
                     (__toplevel_cons
                        (__toplevel_cons
                           'assign
                           (__toplevel_cons
                              'continue
                              (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons _proc-return0 ())) ())))
                        (__toplevel_cons
                           (__toplevel_cons
                              'assign
                              (__toplevel_cons
                                 'val
                                 (__toplevel_cons
                                    (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                    (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                           (__toplevel_cons
                              (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                              (__toplevel_cons
                                 _proc-return0
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'assign
                                       (__toplevel_cons _target0 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'goto
                                          (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons _linkage0 ())) ()))
                                       ()))))))))
               (if (if (eq? _target0 'val) (eq? _linkage0 'return) #f)
                  (make-instruction-sequence
                     (__toplevel_cons 'proc (__toplevel_cons 'continue ()))
                     all-regs
                     (__toplevel_cons
                        (__toplevel_cons
                           'assign
                           (__toplevel_cons
                              'val
                              (__toplevel_cons
                                 (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                        (__toplevel_cons
                           (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                           ())))
                  (if (if (not (eq? _target0 'val)) (eq? _linkage0 'return) #f)
                     (error "return linkage, target not val -- COMPILE" _target0)
                     #f)))))))
 
(define all-regs (__toplevel_cons
      'env
      (__toplevel_cons
         'proc
         (__toplevel_cons 'val (__toplevel_cons 'argl (__toplevel_cons 'continue ()))))))
 
(define registers-needed (lambda (s)
      (if (symbol? s) () (car s))))
 
(define registers-modified (lambda (s)
      (if (symbol? s) () (cadr s))))
 
(define statements (lambda (s)
      (if (symbol? s) (cons s ()) (caddr s))))
 
(define needs-register? (<change>
      (lambda (seq reg)
         (memq reg (registers-needed seq)))
      (lambda (_seq0 _reg0)
         (memq _reg0 (registers-needed _seq0)))))
 
(define modifies-register? (<change>
      (lambda (seq reg)
         (memq reg (registers-modified seq)))
      (lambda (_seq0 _reg0)
         (memq _reg0 (registers-modified _seq0)))))
 
(define append-instruction-sequences (lambda (seqs)
      (define append-2-sequences (lambda (seq1 seq2)
            (make-instruction-sequence
               (list-union
                  (registers-needed seq1)
                  (list-difference (registers-needed seq2) (registers-modified seq1)))
               (list-union (registers-modified seq1) (registers-modified seq2))
               (append (statements seq1) (statements seq2)))))
      (define append-seq-list (<change>
            (lambda (seqs)
               (if (null? seqs)
                  (empty-instruction-sequence)
                  (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))
            (lambda (_seqs0)
               (if (null? _seqs0)
                  (empty-instruction-sequence)
                  (append-2-sequences (car _seqs0) (append-seq-list (cdr _seqs0)))))))
      (append-seq-list seqs)))
 
(define list-union (<change>
      (lambda (s1 s2)
         (if (null? s1)
            s2
            (if (memq (car s1) s2)
               (list-union (cdr s1) s2)
               (cons (car s1) (list-union (cdr s1) s2)))))
      (lambda (_s10 _s20)
         (if (null? _s10)
            _s20
            (if (memq (car _s10) _s20)
               (list-union (cdr _s10) _s20)
               (cons (car _s10) (list-union (cdr _s10) _s20)))))))
 
(define list-difference (lambda (s1 s2)
      (if (null? s1)
         ()
         (if (memq (car s1) s2)
            (list-difference (cdr s1) s2)
            (cons (car s1) (list-difference (cdr s1) s2))))))
 
(define preserving (<change>
      (lambda (regs seq1 seq2)
         (if (null? regs)
            (append-instruction-sequences (cons seq1 (cons seq2 ())))
            (let ((first-reg (car regs)))
               (if (if (needs-register? seq2 first-reg) (modifies-register? seq1 first-reg) #f)
                  (preserving
                     (cdr regs)
                     (make-instruction-sequence
                        (list-union (cons first-reg ()) (registers-needed seq1))
                        (list-difference (registers-modified seq1) (cons first-reg ()))
                        (append
                           (__toplevel_cons (__toplevel_cons 'save (__toplevel_cons first-reg ())) ())
                           (statements seq1)
                           (__toplevel_cons (__toplevel_cons 'restore (__toplevel_cons first-reg ())) ())))
                     seq2)
                  (preserving (cdr regs) seq1 seq2)))))
      (lambda (_regs0 _seq10 _seq20)
         (if (null? _regs0)
            (append-instruction-sequences (cons _seq10 (cons _seq20 ())))
            (let ((_first-reg0 (car _regs0)))
               (if (if (needs-register? _seq20 _first-reg0) (modifies-register? _seq10 _first-reg0) #f)
                  (preserving
                     (cdr _regs0)
                     (make-instruction-sequence
                        (list-union (cons _first-reg0 ()) (registers-needed _seq10))
                        (list-difference (registers-modified _seq10) (cons _first-reg0 ()))
                        (append
                           (__toplevel_cons (__toplevel_cons 'save (__toplevel_cons _first-reg0 ())) ())
                           (statements _seq10)
                           (__toplevel_cons (__toplevel_cons 'restore (__toplevel_cons _first-reg0 ())) ())))
                     _seq20)
                  (preserving (cdr _regs0) _seq10 _seq20)))))))
 
(define tack-on-instruction-sequence (lambda (seq body-seq)
      (make-instruction-sequence
         (registers-needed seq)
         (registers-modified seq)
         (append (statements seq) (statements body-seq)))))
 
(define parallel-instruction-sequences (<change>
      (lambda (seq1 seq2)
         (make-instruction-sequence
            (list-union (registers-needed seq1) (registers-needed seq2))
            (list-union (registers-modified seq1) (registers-modified seq2))
            (append (statements seq1) (statements seq2))))
      (lambda (_seq10 _seq20)
         (make-instruction-sequence
            (list-union (registers-needed _seq10) (registers-needed _seq20))
            (list-union (registers-modified _seq10) (registers-modified _seq20))
            (append (statements _seq10) (statements _seq20))))))
 
(let ((result1 (compile (__toplevel_cons 'define (__toplevel_cons 'x (__toplevel_cons 4 ()))) 'val 'return))
      (expected-result1 (__toplevel_cons
                          (__toplevel_cons 'env (__toplevel_cons 'continue ()))
                          (__toplevel_cons
                             (__toplevel_cons 'val ())
                             (__toplevel_cons
                                (__toplevel_cons
                                   (__toplevel_cons
                                      'assign
                                      (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 4 ())) ())))
                                   (__toplevel_cons
                                      (__toplevel_cons
                                         'perform
                                         (__toplevel_cons
                                            (__toplevel_cons 'op (__toplevel_cons 'define-variable! ()))
                                            (__toplevel_cons
                                               (__toplevel_cons 'const (__toplevel_cons 'x ()))
                                               (__toplevel_cons
                                                  (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                  (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                      (__toplevel_cons
                                         (__toplevel_cons
                                            'assign
                                            (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 'ok ())) ())))
                                         (__toplevel_cons
                                            (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                            ()))))
                                ()))))
      (result2 (compile
                 (__toplevel_cons
                    'lambda
                    (__toplevel_cons
                       (__toplevel_cons 'x (__toplevel_cons 'y ()))
                       (__toplevel_cons (__toplevel_cons '* (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                 'next
                 'val))
      (expected-result2 (__toplevel_cons
                          (__toplevel_cons 'env ())
                          (__toplevel_cons
                             (__toplevel_cons 'next ())
                             (__toplevel_cons
                                (__toplevel_cons
                                   (__toplevel_cons
                                      'assign
                                      (__toplevel_cons
                                         'next
                                         (__toplevel_cons
                                            (__toplevel_cons 'op (__toplevel_cons 'make-compiled-procedure ()))
                                            (__toplevel_cons
                                               (__toplevel_cons 'label (__toplevel_cons 'entry1 ()))
                                               (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                   (__toplevel_cons
                                      (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'val ())) ()))
                                      (__toplevel_cons
                                         'entry1
                                         (__toplevel_cons
                                            (__toplevel_cons
                                               'assign
                                               (__toplevel_cons
                                                  'env
                                                  (__toplevel_cons
                                                     (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-env ()))
                                                     (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                            (__toplevel_cons
                                               (__toplevel_cons
                                                  'assign
                                                  (__toplevel_cons
                                                     'env
                                                     (__toplevel_cons
                                                        (__toplevel_cons 'op (__toplevel_cons 'extend-environment ()))
                                                        (__toplevel_cons
                                                           (__toplevel_cons 'const (__toplevel_cons (__toplevel_cons 'x (__toplevel_cons 'y ())) ()))
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'reg (__toplevel_cons 'argl ()))
                                                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ()))))))
                                               (__toplevel_cons
                                                  (__toplevel_cons
                                                     'assign
                                                     (__toplevel_cons
                                                        'proc
                                                        (__toplevel_cons
                                                           (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'const (__toplevel_cons '* ()))
                                                              (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                  (__toplevel_cons
                                                     (__toplevel_cons
                                                        'assign
                                                        (__toplevel_cons
                                                           'val
                                                           (__toplevel_cons
                                                              (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'const (__toplevel_cons 'y ()))
                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                     (__toplevel_cons
                                                        (__toplevel_cons
                                                           'assign
                                                           (__toplevel_cons
                                                              'argl
                                                              (__toplevel_cons
                                                                 (__toplevel_cons 'op (__toplevel_cons 'list ()))
                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))))
                                                        (__toplevel_cons
                                                           (__toplevel_cons
                                                              'assign
                                                              (__toplevel_cons
                                                                 'val
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons 'op (__toplevel_cons 'lookup-variable-value ()))
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'const (__toplevel_cons 'x ()))
                                                                       (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'env ())) ())))))
                                                           (__toplevel_cons
                                                              (__toplevel_cons
                                                                 'assign
                                                                 (__toplevel_cons
                                                                    'argl
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'op (__toplevel_cons 'cons ()))
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                                          (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                              (__toplevel_cons
                                                                 (__toplevel_cons
                                                                    'test
                                                                    (__toplevel_cons
                                                                       (__toplevel_cons 'op (__toplevel_cons 'primitive-procedure? ()))
                                                                       (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ())))
                                                                 (__toplevel_cons
                                                                    (__toplevel_cons
                                                                       'branch
                                                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'primitive-branch3 ())) ()))
                                                                    (__toplevel_cons
                                                                       'compiled-branch4
                                                                       (__toplevel_cons
                                                                          (__toplevel_cons
                                                                             'assign
                                                                             (__toplevel_cons
                                                                                'val
                                                                                (__toplevel_cons
                                                                                   (__toplevel_cons 'op (__toplevel_cons 'compiled-procedure-entry ()))
                                                                                   (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'proc ())) ()))))
                                                                          (__toplevel_cons
                                                                             (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ()))
                                                                             (__toplevel_cons
                                                                                'primitive-branch3
                                                                                (__toplevel_cons
                                                                                   (__toplevel_cons
                                                                                      'assign
                                                                                      (__toplevel_cons
                                                                                         'val
                                                                                         (__toplevel_cons
                                                                                            (__toplevel_cons 'op (__toplevel_cons 'apply-primitive-procedure ()))
                                                                                            (__toplevel_cons
                                                                                               (__toplevel_cons 'reg (__toplevel_cons 'proc ()))
                                                                                               (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'argl ())) ())))))
                                                                                   (__toplevel_cons
                                                                                      (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                      (__toplevel_cons 'after-call5 (__toplevel_cons 'after-lambda2 ()))))))))))))))))))))
                                ())))))
   (if (equal? result1 expected-result1)
      (equal? result2 expected-result2)
      #f))
 
