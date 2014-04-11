;;Parser Function;;

(load "~/Documents/Compilers/project/get_expr_from_tokens.scm")

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

;;; var

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
;(define (cond? expr)
;(and (pair? expr) (equal? (car expr) 'cond) (pair? (cdr expr)) (not (null? (cdr expr))) (cond?-helper (cdr expr))))

;(define (parse-cond expr)
;  ()
;)


;;if;;
;(define (if expr)
;  (and (pair? expr) (equal? (car expr) 'if) (not (null? (cdr expr)))(not (null? (cddr expr)))(null? (cdddr expr)))
;)

;(define (parse-if expr)
;  ()
;)

;;ifelse;;
;(define (ifelse? expr)
;  (and (pair? expr) (equal? (car expr) 'if) (not (null? (cdr expr)))(not (null? (cddr expr)))(not (null? (cdddr expr)))(null? (cddddr expr)))
;)
;(define (parse-ifelse expr)
;  ()
;)


;;;begin

(define begin?
  (lambda (sexpr)
    (and (pair? sexpr)(equal? (car sexpr) 'begin)(not (null? (cdr sexpr))))))

(define parse-begin
  (lambda (sexpr)
    (list 'seq (parse-applic-args (cdr sexpr)))))

(define beginify
  (lambda (sexpr)
    (cond ((null? sexpr) void-object)
          ((null? (cdr sexpr)) (car sexpr))
          (else `(begin ,@sexpr) ))))


;;or;;
(define (or? expr)
  (and (pair? expr)(equal? (car expr) 'or))
)

(define (parse-or expr)
  (cond ((null? (cdr expr)) (parse '#f))
        (else (list 'or (map parse (cdr expr))))
  )
)

;;and;;
;(define (and? expr)
;  ()
;)

;;parse function;;
(define (parse expr)
  (cond
   ((const? expr) (parse-const expr))
   ((quote? expr) (parse-quote expr))
   ((var? expr) (parse-var expr))
    ((define? expr) (parse-define expr))
    ((or? expr) (parse-or expr))
    ((lambda? expr) (parse-lambda expr))
    ((begin? expr) (parse-begin expr))
   ;((and? expr) (parse-and expr))
  ; ((if? expr) (parse-if expr))
  ; ((ifelse? expr) (parse-ifelse expr))
  ; ((cond? expr) (parse-cond expr))
   (else (display "Doesn't fit any category"))
   )
)
