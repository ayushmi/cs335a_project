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
  ()
)

;(define (define? expr)
;  (list 'define (parse (cadr expr)) (parse ( ))) ;;;;;doubt
;)

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
  ; ((define? expr) (parse-define expr))
   ((or? expr) (parse-or expr))
   ;((and? expr) (parse-and expr))
   (else (display "Doesn't fit any category"))
   )
)
