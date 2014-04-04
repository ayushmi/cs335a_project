;;Parser Function;;

(load "~/Documents/Compilers/project/lexer.scm")

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


;;define;;
(define (define? expr)
  (and (pair? sexpr)(equal? (car sexpr) 'define)(pair? (cdr sexpr))(pair? (cddr sexpr)) (or (and (var? (cadr sexpr)) (null? (cdddr sexpr))) (and (list? (cadr sexpr)) (var? (caadr sexpr)))))
)

(define (parse-define expr)
  (list 'define (parse (cadr expr)) (parse ( ))) ;;;;; include expand also
)


;;cond;;
(define (cond? expr)
  (and (pair? sexpr) (equal? (car sexpr) 'cond) (pair? (cdr sexpr)) (not (null? (cdr sexpr))) (cond?-helper (cdr sexpr)))))
)
(define (parse-cond expr)
  ()
)


;;if;;
(define (if expr)
  (and (pair? sexpr) (equal? (car sexpr) 'if) (not (null? (cdr sexpr)))(not (null? (cddr sexpr)))(null? (cdddr sexpr)))
)

(define (parse-if expr)
  ()
)

;;ifelse;;
(define (ifelse? expr)
  (and (pair? sexpr) (equal? (car sexpr) 'if) (not (null? (cdr sexpr)))(not (null? (cddr sexpr)))(not (null? (cdddr sexpr)))(null? (cddddr sexpr)))
)
(define (parse-ifelse expr)
  ()
)




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
(define (and? expr)
  ()
)

;;parse function;;
(define (parse expr)
  (cond
   ((const? expr) (parse-const expr))
   ((quote? expr) (parse-quote expr))
  ; ((var? expr) (parse-var expr))
    ((define? expr) (parse-define expr))
    ((or? expr) (parse-or expr))
   ;((and? expr) (parse-and expr))
   ((if? expr) (parse-if expr))
   ((ifelse? expr) (parse-ifelse expr))
   ((cond? expr) (parse-cond expr))
   (else (display "Doesn't fit any category"))
   )
)
