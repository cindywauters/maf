;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((map (lambda (f lst)
                   (if (pair? lst)
                      (cons (f (car lst)) (map f (cdr lst)))
                      ())))
            (append (lambda (lst1 lst2)
                      (if (not (pair? lst1))
                         lst2
                         (cons (car lst1) (append (cdr lst1) lst2)))))
            (string->list (lambda (s)
                            (letrec ((f (lambda (i)
                                          (if (< i (string-length s))
                                             (cons (string-ref s i) (f (+ i 1)))
                                             ()))))
                               (f 0))))
            (tagged-list? (lambda (tag l)
                            (if (pair? l) (eq? tag (car l)) #f)))
            (char->natural (lambda (c)
                             (let ((i (char->integer c)))
                                (if (< i 0) (* -2 i) (+ (* 2 i) 1)))))
            (integer->char-list (lambda (n)
                                  (string->list (number->string n))))
            (const? (lambda (exp)
                      (integer? exp)))
            (ref? (lambda (exp)
                    (symbol? exp)))
            (let? (lambda (exp)
                    (tagged-list? 'let exp)))
            (let->bindings (lambda (exp)
                             (cadr exp)))
            (let->exp (lambda (exp)
                        (caddr exp)))
            (letrec1? (lambda (exp)
                        (if (tagged-list? 'letrec exp)
                           (= (length (cadr exp)) 1)
                           #f)))
            (letrec1->binding (lambda (exp)
                                (caadr exp)))
            (letrec1->exp (lambda (exp)
                            (caddr exp)))
            (lambda? (lambda (exp)
                       (tagged-list? 'lambda exp)))
            (lambda->formals (lambda (exp)
                               (cadr exp)))
            (lambda->exp (lambda (exp)
                           (caddr exp)))
            (if? (lambda (exp)
                   (tagged-list? 'if exp)))
            (if->condition (lambda (exp)
                             (cadr exp)))
            (if->then (lambda (exp)
                        (caddr exp)))
            (if->else (lambda (exp)
                        (cadddr exp)))
            (app? (lambda (exp)
                    (pair? exp)))
            (app->fun (lambda (exp)
                        (car exp)))
            (app->args (lambda (exp)
                         (cdr exp)))
            (prim? (lambda (exp)
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
                                          (if __or_res __or_res (eq? exp 'display)))))))))))
            (begin? (lambda (exp)
                      (tagged-list? 'begin exp)))
            (begin->exps (lambda (exp)
                           (cdr exp)))
            (set!? (lambda (exp)
                     (tagged-list? 'set! exp)))
            (set!-var (lambda (exp)
                        (cadr exp)))
            (set!-exp (lambda (exp)
                        (caddr exp)))
            (let=>lambda (lambda (exp)
                           (if (let? exp)
                              (let ((vars (map car (let->bindings exp)))
                                    (args (map cadr (let->bindings exp))))
                                 (cons (cons 'lambda (cons vars (cons (let->exp exp) ()))) args))
                              exp)))
            (arity (lambda (lam)
                     (length (lambda->formals lam))))
            (xargs (lambda (n)
                     (if (<= n 0)
                        ()
                        (cons (string->symbol (string-append "x" (number->string n))) (xargs (- n 1))))))
            (Yn (lambda (n)
                  (cons
                     (cons
                        'lambda
                        (cons
                           (cons 'h ())
                           (cons
                              (cons
                                 'lambda
                                 (cons
                                    (cons 'F ())
                                    (cons
                                       (cons
                                          'F
                                          (cons
                                             (cons
                                                'lambda
                                                (cons (xargs n) (cons (cons (cons (cons 'h (cons 'h ())) (cons 'F ())) (xargs n)) ())))
                                             ()))
                                       ())))
                              ())))
                     (cons
                        (cons
                           'lambda
                           (cons
                              (cons 'h ())
                              (cons
                                 (cons
                                    'lambda
                                    (cons
                                       (cons 'F ())
                                       (cons
                                          (cons
                                             'F
                                             (cons
                                                (cons
                                                   'lambda
                                                   (cons (xargs n) (cons (cons (cons (cons 'h (cons 'h ())) (cons 'F ())) (xargs n)) ())))
                                                ()))
                                          ())))
                                 ())))
                        ()))))
            (letrec1=>Y (lambda (exp)
                          (if (letrec1? exp)
                             (let* ((binding (letrec1->binding exp))
                                    (name (car binding))
                                    (arg (cadr binding))
                                    (num-args (arity arg)))
                                (cons
                                   'let
                                   (cons
                                      (cons
                                         (cons
                                            name
                                            (cons (cons (Yn num-args) (cons (cons 'lambda (cons (cons name ()) (cons arg ()))) ())) ()))
                                         ())
                                      (cons (letrec1->exp exp) ()))))
                             exp)))
            (singlet? (lambda (l)
                        (if (list? l) (= (length l) 1) #f)))
            (dummy-bind (lambda (exps)
                          (if (singlet? exps)
                             (car exps)
                             (if (pair? exps)
                                (cons 'let (cons (cons (cons '$_ (cons (car exps) ())) ()) (cons (dummy-bind (cdr exps)) ())))
                                (error "no match")))))
            (begin=>let (lambda (exp)
                          (dummy-bind (begin->exps exp))))
            (mutable-variables ())
            (mark-mutable (lambda (symbol)
                            (set! mutable-variables (cons symbol mutable-variables))))
            (is-in? (lambda (S symbol)
                      (if (not (pair? S))
                         #f
                         (if (eq? (car S) symbol)
                            #t
                            (is-in? (cdr S) symbol)))))
            (is-mutable? (lambda (symbol)
                           (is-in? mutable-variables symbol)))
            (analyze-mutable-variables (lambda (exp)
                                         (if (const? exp)
                                            #f
                                            (if (ref? exp)
                                               #f
                                               (if (prim? exp)
                                                  #f
                                                  (if (lambda? exp)
                                                     (analyze-mutable-variables (lambda->exp exp))
                                                     (if (let? exp)
                                                        (begin
                                                           (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                                           (analyze-mutable-variables (let->exp exp)))
                                                        (if (letrec1? exp)
                                                           (begin
                                                              (analyze-mutable-variables (cadr (letrec1->binding exp)))
                                                              (analyze-mutable-variables (letrec1->exp exp)))
                                                           (if (set!? exp)
                                                              (mark-mutable (set!-var exp))
                                                              (if (if? exp)
                                                                 (begin
                                                                    (analyze-mutable-variables (if->condition exp))
                                                                    (analyze-mutable-variables (if->then exp))
                                                                    (analyze-mutable-variables (if->else exp)))
                                                                 (if (begin? exp)
                                                                    (begin
                                                                       (map analyze-mutable-variables (begin->exps exp))
                                                                       #f)
                                                                    (if (app? exp)
                                                                       (begin
                                                                          (map analyze-mutable-variables exp)
                                                                          #f)
                                                                       (error "unknown expression type: " exp)))))))))))))
            (m (lambda (chars)
                 (if (null? chars)
                    ()
                    (cons (car chars) (m (cdr chars))))))
            (mangle (lambda (symbol)
                      (list->string (m (string->list (symbol->string symbol))))))
            (java-compile-const (lambda (exp)
                                  (if (integer? exp)
                                     (string-append "new IntValue(" (number->string exp) ")")
                                     (error "unknown constant: " exp))))
            (java-compile-prim (lambda (p)
                                 (if (eq? '+ p)
                                    "sum"
                                    (if (eq? '- p)
                                       "difference"
                                       (if (eq? '* p)
                                          "product"
                                          (if (eq? '= p)
                                             "numEqual"
                                             (if (eq? 'display p)
                                                "display"
                                                (error "unhandled primitive " p))))))))
            (java-compile-ref (lambda (exp)
                                (if (is-mutable? exp)
                                   (string-append "m_" (mangle exp) ".value")
                                   (mangle exp))))
            (java-compile-formals (lambda (formals)
                                    (if (not (pair? formals))
                                       ""
                                       (string-append
                                          "final Value "
                                          (mangle (car formals))
                                          (if (pair? (cdr formals))
                                             (string-append ", " (java-compile-formals (cdr formals)))
                                             "")))))
            (java-wrap-mutables (lambda (vars)
                                  (if (not (pair? vars))
                                     ""
                                     (string-append
                                        (if (is-mutable? (car vars))
                                           (string-append
                                              " final ValueCell m_"
                                              (mangle (car vars))
                                              " = new ValueCell("
                                              (mangle (car vars))
                                              ");\n")
                                           "")
                                        (java-wrap-mutables (cdr vars))))))
            (java-compile-lambda (lambda (exp)
                                   (let* ((formals (lambda->formals exp))
                                          (num-args (length formals)))
                                      (string-append
                                         "new NullProcValue"
                                         (number->string num-args)
                                         " () {\n"
                                         " public Value apply("
                                         (java-compile-formals formals)
                                         ") {\n"
                                         (java-wrap-mutables formals)
                                         "\n"
                                         "  return "
                                         (java-compile-exp (lambda->exp exp))
                                         " ;\n"
                                         "}}\n"))))
            (java-compile-args (lambda (args)
                                 (if (not (pair? args))
                                    ""
                                    (string-append
                                       (java-compile-exp (car args))
                                       (if (pair? (cdr args))
                                          (string-append ", " (java-compile-args (cdr args)))
                                          "")))))
            (java-compile-set! (lambda (exp)
                                 (string-append
                                    "VoidValue.Void(m_"
                                    (mangle (set!-var exp))
                                    ".value = "
                                    (java-compile-exp (set!-exp exp))
                                    ")")))
            (java-compile-app (lambda (exp)
                                (let* ((args (app->args exp))
                                       (fun (app->fun exp))
                                       (num-args (length args)))
                                   (string-append
                                      "((ProcValue"
                                      (number->string num-args)
                                      ")("
                                      (java-compile-exp fun)
                                      ")).apply("
                                      (java-compile-args args)
                                      ")\n"))))
            (java-compile-if (lambda (exp)
                               (string-append
                                  "("
                                  (java-compile-exp (if->condition exp))
                                  ").toBoolean() ? ("
                                  (java-compile-exp (if->then exp))
                                  ") : ("
                                  (java-compile-exp (if->else exp))
                                  ")")))
            (java-compile-exp (lambda (exp)
                                (if (const? exp)
                                   (java-compile-const exp)
                                   (if (prim? exp)
                                      (java-compile-prim exp)
                                      (if (ref? exp)
                                         (java-compile-ref exp)
                                         (if (lambda? exp)
                                            (java-compile-lambda exp)
                                            (if (if? exp)
                                               (java-compile-if exp)
                                               (if (set!? exp)
                                                  (java-compile-set! exp)
                                                  (if (let? exp)
                                                     (java-compile-exp (let=>lambda exp))
                                                     (if (letrec1? exp)
                                                        (java-compile-exp (letrec1=>Y exp))
                                                        (if (begin? exp)
                                                           (java-compile-exp (begin=>let exp))
                                                           (if (app? exp)
                                                              (java-compile-app exp)
                                                              (error "no match")))))))))))))
            (java-compile-program (lambda (exp)
                                    (string-append
                                       "public class BOut extends RuntimeEnvironment {\n"
                                       " public static void main (String[] args) {\n"
                                       (java-compile-exp exp)
                                       " ;\n"
                                       " }\n"
                                       "}\n")))
            (input-program 3))
      (analyze-mutable-variables input-program)
      (java-compile-program input-program))
   (letrec ((_map0 (lambda (_f0 _lst0)
                     (if (pair? _lst0)
                        (cons (_f0 (car _lst0)) (_map0 _f0 (cdr _lst0)))
                        ())))
            (_append0 (lambda (_lst10 _lst20)
                        (if (not (pair? _lst10))
                           _lst20
                           (cons (car _lst10) (_append0 (cdr _lst10) _lst20)))))
            (_string->list0 (lambda (_s0)
                              (letrec ((_f1 (lambda (_i0)
                                              (if (< _i0 (string-length _s0))
                                                 (cons (string-ref _s0 _i0) (_f1 (+ _i0 1)))
                                                 ()))))
                                 (_f1 0))))
            (_tagged-list?0 (lambda (_tag0 _l0)
                              (if (pair? _l0) (eq? _tag0 (car _l0)) #f)))
            (_char->natural0 (lambda (_c0)
                               (let ((_i1 (char->integer _c0)))
                                  (if (< _i1 0) (* -2 _i1) (+ (* 2 _i1) 1)))))
            (_integer->char-list0 (lambda (_n0)
                                    (_string->list0 (number->string _n0))))
            (_const?0 (lambda (_exp0)
                        (integer? _exp0)))
            (_ref?0 (lambda (_exp1)
                      (symbol? _exp1)))
            (_let?0 (lambda (_exp2)
                      (_tagged-list?0 'let _exp2)))
            (_let->bindings0 (lambda (_exp3)
                               (cadr _exp3)))
            (_let->exp0 (lambda (_exp4)
                          (caddr _exp4)))
            (_letrec1?0 (lambda (_exp5)
                          (if (_tagged-list?0 'letrec _exp5)
                             (= (length (cadr _exp5)) 1)
                             #f)))
            (_letrec1->binding0 (lambda (_exp6)
                                  (caadr _exp6)))
            (_letrec1->exp0 (lambda (_exp7)
                              (caddr _exp7)))
            (_lambda?0 (lambda (_exp8)
                         (_tagged-list?0 'lambda _exp8)))
            (_lambda->formals0 (lambda (_exp9)
                                 (cadr _exp9)))
            (_lambda->exp0 (lambda (_exp10)
                             (caddr _exp10)))
            (_if?0 (lambda (_exp11)
                     (_tagged-list?0 'if _exp11)))
            (_if->condition0 (lambda (_exp12)
                               (cadr _exp12)))
            (_if->then0 (lambda (_exp13)
                          (caddr _exp13)))
            (_if->else0 (lambda (_exp14)
                          (cadddr _exp14)))
            (_app?0 (lambda (_exp15)
                      (pair? _exp15)))
            (_app->fun0 (lambda (_exp16)
                          (car _exp16)))
            (_app->args0 (lambda (_exp17)
                           (cdr _exp17)))
            (_prim?0 (lambda (_exp18)
                       (let ((___or_res0 (eq? _exp18 '+)))
                          (if ___or_res0
                             ___or_res0
                             (let ((___or_res1 (eq? _exp18 '-)))
                                (if ___or_res1
                                   ___or_res1
                                   (let ((___or_res2 (eq? _exp18 '*)))
                                      (if ___or_res2
                                         ___or_res2
                                         (let ((___or_res3 (eq? _exp18 '=)))
                                            (if ___or_res3 ___or_res3 (eq? _exp18 'display)))))))))))
            (_begin?0 (lambda (_exp19)
                        (_tagged-list?0 'begin _exp19)))
            (_begin->exps0 (lambda (_exp20)
                             (cdr _exp20)))
            (_set!?0 (lambda (_exp21)
                       (_tagged-list?0 'set! _exp21)))
            (_set!-var0 (lambda (_exp22)
                          (cadr _exp22)))
            (_set!-exp0 (lambda (_exp23)
                          (caddr _exp23)))
            (_let=>lambda0 (lambda (_exp24)
                             (if (_let?0 _exp24)
                                (let ((_vars0 (_map0 car (_let->bindings0 _exp24)))
                                      (_args0 (_map0 cadr (_let->bindings0 _exp24))))
                                   (cons (cons 'lambda (cons _vars0 (cons (_let->exp0 _exp24) ()))) _args0))
                                _exp24)))
            (_arity0 (lambda (_lam0)
                       (length (_lambda->formals0 _lam0))))
            (_xargs0 (lambda (_n1)
                       (if (<= _n1 0)
                          ()
                          (cons (string->symbol (string-append "x" (number->string _n1))) (_xargs0 (- _n1 1))))))
            (_Yn0 (lambda (_n2)
                    (cons
                       (cons
                          'lambda
                          (cons
                             (cons 'h ())
                             (cons
                                (cons
                                   'lambda
                                   (cons
                                      (cons 'F ())
                                      (cons
                                         (cons
                                            'F
                                            (cons
                                               (cons
                                                  'lambda
                                                  (cons (_xargs0 _n2) (cons (cons (cons (cons 'h (cons 'h ())) (cons 'F ())) (_xargs0 _n2)) ())))
                                               ()))
                                         ())))
                                ())))
                       (cons
                          (cons
                             'lambda
                             (cons
                                (cons 'h ())
                                (cons
                                   (cons
                                      'lambda
                                      (cons
                                         (cons 'F ())
                                         (cons
                                            (cons
                                               'F
                                               (cons
                                                  (cons
                                                     'lambda
                                                     (cons (_xargs0 _n2) (cons (cons (cons (cons 'h (cons 'h ())) (cons 'F ())) (_xargs0 _n2)) ())))
                                                  ()))
                                            ())))
                                   ())))
                          ()))))
            (_letrec1=>Y0 (lambda (_exp25)
                            (if (_letrec1?0 _exp25)
                               (let* ((_binding0 (_letrec1->binding0 _exp25))
                                      (_name0 (car _binding0))
                                      (_arg0 (cadr _binding0))
                                      (_num-args0 (_arity0 _arg0)))
                                  (cons
                                     'let
                                     (cons
                                        (cons
                                           (cons
                                              _name0
                                              (cons (cons (_Yn0 _num-args0) (cons (cons 'lambda (cons (cons _name0 ()) (cons _arg0 ()))) ())) ()))
                                           ())
                                        (cons (_letrec1->exp0 _exp25) ()))))
                               _exp25)))
            (_singlet?0 (lambda (_l1)
                          (if (list? _l1) (= (length _l1) 1) #f)))
            (_dummy-bind0 (lambda (_exps0)
                            (if (_singlet?0 _exps0)
                               (car _exps0)
                               (if (pair? _exps0)
                                  (cons
                                     'let
                                     (cons (cons (cons '$_ (cons (car _exps0) ())) ()) (cons (_dummy-bind0 (cdr _exps0)) ())))
                                  (error "no match")))))
            (_begin=>let0 (lambda (_exp26)
                            (_dummy-bind0 (_begin->exps0 _exp26))))
            (_mutable-variables0 ())
            (_mark-mutable0 (lambda (_symbol0)
                              (set! _mutable-variables0 (cons _symbol0 _mutable-variables0))))
            (_is-in?0 (lambda (_S0 _symbol1)
                        (if (not (pair? _S0))
                           #f
                           (if (eq? (car _S0) _symbol1)
                              #t
                              (_is-in?0 (cdr _S0) _symbol1)))))
            (_is-mutable?0 (lambda (_symbol2)
                             (_is-in?0 _mutable-variables0 _symbol2)))
            (_analyze-mutable-variables0 (lambda (_exp27)
                                           (if (_const?0 _exp27)
                                              #f
                                              (if (_ref?0 _exp27)
                                                 #f
                                                 (if (_prim?0 _exp27)
                                                    #f
                                                    (if (_lambda?0 _exp27)
                                                       (_analyze-mutable-variables0 (_lambda->exp0 _exp27))
                                                       (if (_let?0 _exp27)
                                                          (begin
                                                             (_map0 _analyze-mutable-variables0 (_map0 cadr (_let->bindings0 _exp27)))
                                                             (_analyze-mutable-variables0 (_let->exp0 _exp27)))
                                                          (if (_letrec1?0 _exp27)
                                                             (begin
                                                                (_analyze-mutable-variables0 (cadr (_letrec1->binding0 _exp27)))
                                                                (_analyze-mutable-variables0 (_letrec1->exp0 _exp27)))
                                                             (if (_set!?0 _exp27)
                                                                (_mark-mutable0 (_set!-var0 _exp27))
                                                                (if (_if?0 _exp27)
                                                                   (begin
                                                                      (_analyze-mutable-variables0 (_if->condition0 _exp27))
                                                                      (_analyze-mutable-variables0 (_if->then0 _exp27))
                                                                      (_analyze-mutable-variables0 (_if->else0 _exp27)))
                                                                   (if (_begin?0 _exp27)
                                                                      (begin
                                                                         (_map0 _analyze-mutable-variables0 (_begin->exps0 _exp27))
                                                                         #f)
                                                                      (if (_app?0 _exp27)
                                                                         (begin
                                                                            (_map0 _analyze-mutable-variables0 _exp27)
                                                                            #f)
                                                                         (error "unknown expression type: " _exp27)))))))))))))
            (_m0 (lambda (_chars0)
                   (if (null? _chars0)
                      ()
                      (cons (car _chars0) (_m0 (cdr _chars0))))))
            (_mangle0 (lambda (_symbol3)
                        (list->string (_m0 (_string->list0 (symbol->string _symbol3))))))
            (_java-compile-const0 (lambda (_exp28)
                                    (if (integer? _exp28)
                                       (string-append "new IntValue(" (number->string _exp28) ")")
                                       (error "unknown constant: " _exp28))))
            (_java-compile-prim0 (lambda (_p0)
                                   (if (eq? '+ _p0)
                                      "sum"
                                      (if (eq? '- _p0)
                                         "difference"
                                         (if (eq? '* _p0)
                                            "product"
                                            (if (eq? '= _p0)
                                               "numEqual"
                                               (if (eq? 'display _p0)
                                                  "display"
                                                  (error "unhandled primitive " _p0))))))))
            (_java-compile-ref0 (lambda (_exp29)
                                  (if (_is-mutable?0 _exp29)
                                     (string-append "m_" (_mangle0 _exp29) ".value")
                                     (_mangle0 _exp29))))
            (_java-compile-formals0 (lambda (_formals0)
                                      (if (not (pair? _formals0))
                                         ""
                                         (string-append
                                            "final Value "
                                            (_mangle0 (car _formals0))
                                            (if (pair? (cdr _formals0))
                                               (string-append ", " (_java-compile-formals0 (cdr _formals0)))
                                               "")))))
            (_java-wrap-mutables0 (lambda (_vars1)
                                    (if (not (pair? _vars1))
                                       ""
                                       (string-append
                                          (if (_is-mutable?0 (car _vars1))
                                             (string-append
                                                " final ValueCell m_"
                                                (_mangle0 (car _vars1))
                                                " = new ValueCell("
                                                (_mangle0 (car _vars1))
                                                ");\n")
                                             "")
                                          (_java-wrap-mutables0 (cdr _vars1))))))
            (_java-compile-lambda0 (lambda (_exp30)
                                     (let* ((_formals1 (_lambda->formals0 _exp30))
                                            (_num-args1 (length _formals1)))
                                        (string-append
                                           "new NullProcValue"
                                           (number->string _num-args1)
                                           " () {\n"
                                           " public Value apply("
                                           (_java-compile-formals0 _formals1)
                                           ") {\n"
                                           (_java-wrap-mutables0 _formals1)
                                           "\n"
                                           "  return "
                                           (_java-compile-exp0 (_lambda->exp0 _exp30))
                                           " ;\n"
                                           "}}\n"))))
            (_java-compile-args0 (lambda (_args1)
                                   (if (not (pair? _args1))
                                      ""
                                      (string-append
                                         (_java-compile-exp0 (car _args1))
                                         (if (pair? (cdr _args1))
                                            (string-append ", " (_java-compile-args0 (cdr _args1)))
                                            "")))))
            (_java-compile-set!0 (lambda (_exp31)
                                   (string-append
                                      "VoidValue.Void(m_"
                                      (_mangle0 (_set!-var0 _exp31))
                                      ".value = "
                                      (_java-compile-exp0 (_set!-exp0 _exp31))
                                      ")")))
            (_java-compile-app0 (lambda (_exp32)
                                  (let* ((_args2 (_app->args0 _exp32))
                                         (_fun0 (_app->fun0 _exp32))
                                         (_num-args2 (length _args2)))
                                     (string-append
                                        "((ProcValue"
                                        (number->string _num-args2)
                                        ")("
                                        (_java-compile-exp0 _fun0)
                                        ")).apply("
                                        (_java-compile-args0 _args2)
                                        ")\n"))))
            (_java-compile-if0 (lambda (_exp33)
                                 (string-append
                                    "("
                                    (_java-compile-exp0 (_if->condition0 _exp33))
                                    ").toBoolean() ? ("
                                    (_java-compile-exp0 (_if->then0 _exp33))
                                    ") : ("
                                    (_java-compile-exp0 (_if->else0 _exp33))
                                    ")")))
            (_java-compile-exp0 (lambda (_exp34)
                                  (if (_const?0 _exp34)
                                     (_java-compile-const0 _exp34)
                                     (if (_prim?0 _exp34)
                                        (_java-compile-prim0 _exp34)
                                        (if (_ref?0 _exp34)
                                           (_java-compile-ref0 _exp34)
                                           (if (_lambda?0 _exp34)
                                              (_java-compile-lambda0 _exp34)
                                              (if (_if?0 _exp34)
                                                 (_java-compile-if0 _exp34)
                                                 (if (_set!?0 _exp34)
                                                    (_java-compile-set!0 _exp34)
                                                    (if (_let?0 _exp34)
                                                       (_java-compile-exp0 (_let=>lambda0 _exp34))
                                                       (if (_letrec1?0 _exp34)
                                                          (_java-compile-exp0 (_letrec1=>Y0 _exp34))
                                                          (if (_begin?0 _exp34)
                                                             (_java-compile-exp0 (_begin=>let0 _exp34))
                                                             (if (_app?0 _exp34)
                                                                (_java-compile-app0 _exp34)
                                                                (error "no match")))))))))))))
            (_java-compile-program0 (lambda (_exp35)
                                      (string-append
                                         "public class BOut extends RuntimeEnvironment {\n"
                                         " public static void main (String[] args) {\n"
                                         (_java-compile-exp0 _exp35)
                                         " ;\n"
                                         " }\n"
                                         "}\n")))
            (_input-program0 3))
      (_analyze-mutable-variables0 _input-program0)
      (_java-compile-program0 _input-program0)))
 
