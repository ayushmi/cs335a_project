(load "~/Documents/Compilers/project/lexer.scm")

(define tokens->sexprs
  (letrec ((get-sexpr
            (lambda (toks ret-sexpr+toks ret-none)
              (cond ((or (null? toks)
                         (eq? (caar toks) 'rightparen)
                         (eq? (caar toks) 'dot))
                     (ret-none))
;simple form
                    ((ormap (lambda (tag) (eq? (caar toks) tag))
                    '(boolean char number string symbol))
                     (ret-sexpr+toks (cadar toks) (cdr toks)))
;simple quote form
                    ((eq? (caar toks) 'single-quote)
                     (get-sexpr (cdr toks)
                                (lambda (sexpr2 tok2)
                                  (ret-sexpr+toks (list 'quote sexpr2) tok2))
                                (lambda () 'Expected_an_sexpr_after_qoute)))
;other qoute form
                    ((ormap (lambda (tag) (eq? (caar toks) tag))
                            '(quasiquote unquote unquote-splicing))
                     (get-sexpr (cdr toks)
                                (lambda (sexpr2 tok2)
                                  (ret-sexpr+toks (list (caar toks) sexpr2) tok2))
                                (lambda () 'Expected_an_sexpr_after_qouteType)))
;list type
                    ((eq? (caar toks) 'leftparen)
                     (get-sexprs (cdr toks)
                                 (lambda (sexpr2 tok2)
                                   (cond ((null? tok2) ;missing rightparen/dot
                                           (error 'tokens->sexprs
                                                  (format "List type is missing rightparen/dot ~s" toks)))
                                        ;list
                                          ((eq? (caar tok2) 'rightparen)
                                           (ret-sexpr+toks sexpr2 (cdr tok2)))
                                        ;ImproperList
                                          ((eq? (caar tok2) 'dot)
                                           (get-sexpr (cdr tok2)
                                                      (lambda (sexpr3 tok3)
                                                        (if (eq? (caar toks) 'leftparen)
                                                            (ret-sexpr+toks `(,@sexpr2 . ,sexpr3) (cdr tok3))
                                                            (error 'tokens->sexprs
                                                                   (format "ImproperList missing rightparen ~s" toks))))
                                                        (lambda () 'ImproperList missing rightparen))
                                           )))))
;vector
                    ((eq? (caar toks) 'vector)
                     (get-sexprs (cdr toks)
                                 (lambda (sexpr2 tok2)
                                   (cond ((null? tok2) ;missing rightparen
                                           (error 'tokens->sexprs
                                                  (format "vector is missing rightparen ~s" toks)))
                                          ((eq? (caar tok2) 'rightparen)
                                           (ret-sexpr+toks (list->vector sexpr2) (cdr tok2)))
                                          (else (error 'tokens->sexprs
                                                       (format "vector is missing rightparen ~s" toks)))))))
;unreconized form
                    (else (error 'tokens->sexprs
                                (format "I can't recognize an sexpr in ~s" toks))))))

           (get-sexprs
            (lambda (toks ret-sexprs+toks)
              (get-sexpr toks
               (lambda (sexpr toks)
                 (get-sexprs toks
                  (lambda (sexprs toks)
                    (ret-sexprs+toks (cons sexpr sexprs) toks))))
               (lambda () (ret-sexprs+toks '() toks))))))

    (lambda (toks)
      (get-sexprs toks
       (lambda (sexprs toks)
         (if (null? toks)
             sexprs
             'error))))
    ))
