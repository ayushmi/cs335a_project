;;Parser Function;;

(load "get_expr_from_tokens.scm")

;;const;;
(define (const? expr)
  (or (number? expr) (boolean? expr) (char? expr) (string? expr)) ;;;;include vector?
)

(define (parse-const expr)
  (list 'const expr)
)

;;quote;;
(define (quote? expr)
  (and (pair? expr) (equal? (car expr) 'quote))
)

(define (parse-quote expr)
  (list 'const (car (cdr expr)))
)

;;;var

(define parse-var
  (lambda (sexpr)
    (list 'var sexpr)))

(define reserved '(if lambda cond define begin or and let let* letrec quasiquote))

(define non-reserved
  (lambda (sexpr reserved-list)
    (if (null? reserved-list)
        #t
        (if (equal? sexpr (car reserved-list))
            #f
            (non-reserved sexpr (cdr reserved-list))))
    ))

(define var?
  (lambda (sexpr)
    (and (not (vector? sexpr))(not (list? sexpr)) (non-reserved sexpr reserved))))

;define
(define define?
  (lambda (sexpr)
    (and (pair? sexpr)(equal? (car sexpr) 'define)(pair? (cdr sexpr))(pair? (cddr sexpr)) (or (and (var? (cadr sexpr)) (null? (cdddr sexpr)))
                                                                                              (and (list? (cadr sexpr)) (var? (caadr sexpr)))))))

(define parse-define
  (lambda (sexpr)
    (cond ((pair? (cadr sexpr)) (parse (expand-mit sexpr)))
          (else (list 'define (parse (cadr sexpr)) (parse (caddr sexpr)))))))

;;convert to lambda form of function definition;;
(define expand-mit
  (lambda (sexpr)
    `(define ,(caadr sexpr) (lambda ,(cdadr sexpr) ,@(cddr sexpr)))))


;;;lambda

(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e))))
          (cdr e))))))

(define not-dup?
(lambda (e)
(cond ((list? e) (= (length (dedupe e)) (length e)))
(else #t))))

(define lambda?
  (lambda (sexpr)
    (and (pair? sexpr) (equal? (car sexpr) 'lambda)(pair? (cdr sexpr))(pair? (cddr sexpr)) (not-dup? (cadr sexpr)))))

(define parse-lambda-helper
  (lambda (sexpr ret-pro ret-imp ret-sym)
    (cond ((pair? sexpr)
           (parse-lambda-helper (cdr sexpr)
                                ret-pro
                                (lambda (s a) (ret-imp (cons (car sexpr) s) a))
                                (lambda () (ret-imp (list (car sexpr)) (cdr sexpr)))))
          ((null? sexpr) (ret-pro))
          (else (ret-sym)))))

(define parse-lambda
  (lambda (sexpr)
    (let ((arg1 (cadr sexpr))
          ;(body (parse (caddr sexpr))))
          (body (parse (beginify (cddr sexpr)))))
      (parse-lambda-helper arg1
                           (lambda () `(lambda-simple ,arg1 ,body))
                           (lambda (s a) `(lambda-opt ,s ,a ,body))
                           (lambda () `(lambda-variadic ,arg1 ,body))))))



;;cond;;
(define (cond? expr)
  (and (pair? expr) (equal? (car expr) 'cond) (pair? (cdr expr)) (not (null? (cdr expr))) (cond?-helper (cdr expr))))

(define cond?-helper
  (lambda (sexpr)
    (and (pair? sexpr) (not (null? sexpr)) (pair? (car sexpr)) (not (null? (car sexpr)))
         (or (and (equal? (caar sexpr) 'else) (pair? (cdar sexpr)) (not (null? (cdar sexpr))) (null? (cdr sexpr)))
             (not (equal? (caar sexpr) 'else)))
         (or (null? (cdr sexpr)) (cond?-helper (cdr sexpr))))))

(define expand-cond-helper
  (lambda (sexpr)
    (cond ((null? (cdr sexpr))
           (if (equal? (caar sexpr) 'else)
               (beginify (cdar sexpr))
               `(if ,(caar sexpr) ,(beginify (cdar sexpr)))))
          (else `(if ,(caar sexpr) ,(beginify (cdar sexpr)) ,(expand-cond-helper (cdr sexpr)))))
     ))

(define expand-cond
  (lambda (sexpr)
    (expand-cond-helper (cdr sexpr))))



;;if;;
(define (if? expr)
  (and (pair? expr) (equal? (car expr) 'if) (not (null? (cdr expr)))(not (null? (cddr expr)))(null? (cdddr expr)))
)

(define (parse-if expr)
  (list 'if (parse (cadr expr)) (parse (caddr expr)) `(const ,void-object))
)

;;ifelse;;
(define (ifelse? expr)
  (and (pair? expr) (equal? (car expr) 'if) (not (null? (cdr expr)))(not (null? (cddr expr)))(not (null? (cdddr expr)))(null? (cddddr expr)))
)
(define (parse-ifelse expr)
  (list 'if (parse (cadr expr)) (parse (caddr expr)) (parse (cadddr expr)) )
)

;;let;;
(define let?
  (lambda (sexpr)
    (and (pair? sexpr) (equal? (car sexpr) 'let) (pair? (cdr sexpr))(pair? (cddr sexpr))(or (null? (cadr sexpr)) (and (pair? (cadr sexpr)) (andmap (lambda(x)
                                                                                                                       (and (pair? x)
                                                                                                                            (= (length x) 2)))
                                                                                                                     (cadr sexpr)))))))
(define expand-let
 (lambda (sexpr)
   (let ((args (map car (cadr sexpr)))
         (vals (map cadr (cadr sexpr)))
         (body (cddr sexpr)))
     (cond ((and (null? args) (null? vals)) `((lambda ,args ,@body) ))
           ((and (not (null? args)) (not (null? vals))) `((lambda ,args ,@body) ,@vals ))
           (else "error")))))


;;;begin

(define begin?
  (lambda (sexpr)
    (and (pair? sexpr)(equal? (car sexpr) 'begin)(not (null? (cdr sexpr))))))

(define parse-applic-args
  (lambda (sexpr)
    (if (null? sexpr)
        sexpr
        (cons (parse (car sexpr)) (parse-applic-args (cdr sexpr))))))


(define parse-begin
  (lambda (sexpr)
    (parse-applic-args (cdr sexpr))))

(define beginify
  (lambda (sexpr)
    (cond ((null? sexpr) void-object)
          ;((null? (cdr sexpr)) (car sexpr))
          (else `(begin ,@sexpr) ))))

(define void-object (if #f #t))

;;+;;
(define (+? expr)
  (and (pair? expr)(equal? (car expr) '+))

)

(define parse+
    (lambda (s)
        (if (equal? s '(+))
            '(const 0)
            (list '+ (parse (cadr s))
              (parse+ (append '(+) (cddr s)))
            )
        )
    )
)
;;-;;
(define (-? expr)
  (and (pair? expr)(equal? (car expr) '-))
)


(define parse-
    (lambda (s)
        (if (equal? s '(-))
            '(const 0)
            (list '- (parse (cadr s))
              (parse- (append '(-) (cddr s)))
            )
        )
    )
)

;;*;;
(define (*? expr)
  (and (pair? expr)(equal? (car expr) '*))

)


(define parse*
    (lambda (s)
        (if (equal? s '(*))
            '(const 1)
            (list '* (parse (cadr s))
              (parse* (append '(*) (cddr s)))
            )
        )
    )
)

;;>;;
(define (>? expr)
  (and (pair? expr)(equal? (car expr) '>) (not (null? (cdr expr))))

)


(define (parse> expr)
  (list '> (parse (cadr expr)) (parse (caddr expr)))
)

;;<;;
(define (<? expr)
  (and (pair? expr)(equal? (car expr) '<) (not (null? (cdr expr))))

)


(define (parse< expr)
  (list '< (parse (cadr expr)) (parse (caddr expr)))
)

;;=;;
(define (=? expr)
  (and (pair? expr)(equal? (car expr) '=) (not (null? (cdr expr))))

)


(define (parse= expr)
  (list '= (parse (cadr expr)) (parse (caddr expr)))
)


;;or;;
(define (or? expr)
  (and (pair? expr)(equal? (car expr) 'or))
)

(define parse-or
    (lambda (s)
        (if (equal? s '(or))
            (parse '#f)
            (list 'or (parse (cadr s))
              (parse-or (append '(or) (cddr s)))
            )
        )
    )
)


;(define (parse-or expr)
;  (cond ((null? (cdr expr)) (parse '#f))
;        (else (list 'or (map parse (cdr expr))))
;  )
;)

;;display;;
(define (display? expr)
  (and (pair? expr) (equal? (car expr) 'display))
)
(define (parse-display expr)
  (cond ((string? (cadr expr)) (list 'display (list 'string (cdr expr))))
        (else (list 'display (parse (cadr expr))))
  )
)

;;and;;
(define (and? expr)
    (and (pair? expr)(equal? (car expr) 'and))
)


(define expand-and
 (lambda (sexpr)
   (cond ((null? (cdr sexpr)) '(const 1))
         ((null? (cddr sexpr)) (cadr sexpr))
         (else (expand-and-helper (cdr sexpr))))))

(define expand-and-helper
  (lambda (sexpr)
    (cond ((null? (cddr sexpr)) `(if ,(parse (car sexpr)) , (parse (cadr sexpr)), '(const 0)))
           (else `(if ,(parse (car sexpr)) ,(expand-and-helper (cdr sexpr)), '(const 0))
           ))))

;;function call;
(define (functionCall? expr)
  (and (pair? expr) (not (list? (car expr))) (non-reserved (car expr) reserved) (list? (cdr expr)))
)
(define (parse-functionCall expr)
  (list 'apply (parse (car expr)) (map parse (cdr expr)))
)

;;parse function;;
(define (parse expr)
  (cond

   ((let? expr) (expand-let expr))
   ((const? expr) (parse-const expr))
   ((quote? expr) (parse-quote expr))
   ((+? expr) (parse+ expr))
   ((-? expr) (parse- expr))
   ((*? expr) (parse* expr))
   ((=? expr) (parse= expr))
   ((>? expr) (parse> expr))
   ((<? expr) (parse< expr))
   ((var? expr) (parse-var expr))
   ((define? expr) (parse-define expr))
   ((or? expr) (parse-or expr))
   ((lambda? expr) (parse-lambda expr))
   ((begin? expr) (parse-begin expr))
   ((and? expr) (expand-and expr))
   ((if? expr) (parse-if expr))
   ((ifelse? expr) (parse-ifelse expr))
   ((cond? expr) (expand-cond expr))
   ((display? expr) (parse-display expr))
   ((functionCall? expr) (parse-functionCall expr))
   (else (display "Doesn't fit any category"))
  )
)


;;parse program;;
(define (parse-program in out)
  (cond
    ((null? in) out)
    (else (append (append out (list (parse (car in)))) (parse-program (cdr in) out))
          )
    )
)

;;Test Function ;;
(define (lexParse program)
    (parse-program (tokens->sexprs (tokens program)) '())
)
