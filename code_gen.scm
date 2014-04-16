(load "tests-driver.scm")
(load "parser.scm")

;----------------------------------------------------------------------------------
; #defines
(define wordsize 4)

(define global-env '())

(define (extend-global-env arg1 arg2) ; arg2 is function name
  (set! global-env (cons (list arg2 arg1) global-env))
  )


;----------------------------------------------------------------------------------
; Stack operations
(define (emit-stack-load sp)
  (emit "li $a0,~s" sp)
  )

(define (next-stack-index sp)
  (- sp wordsize)
  )
;---------------------------------------------------------------------------------
; Main Function
(define (main E)
  (define sp 0)
  (define env '())
  (emit ".text\nmain:")
  (driver E sp env)
  (emit "li $v0,10\n")
  (emit "syscall\n")
  (collect-data-section '() 1)
)

;---------------------------DRIVER--------------------------------
(define (driver E sp env)
  (cond
    [(null? E) '()]
    [(null? (cdr E)) (emit-expr (car E) sp env)]
    [else (emit-expr (car E) sp env) (driver (cdr E) sp env)]
    )
)

;---------------------------------------------------------------------------------
; Top level function    EMIT-EXPR
(define (emit-expr E sp env)
  (cond
    ((imm-cg? E) (emit-imm E sp env))
    ((if-cg? E)  (emit-cond E sp env))
    ((let-cg? E) (emit-let E sp env))
    ((define-cg? E) (emit-define E sp env))
    ((display-cg? E) (emit-display E sp env))
    ((lambda-cg? E) (emit-lambda E sp env (unique-label)))
    ((equal? (car E) '*) (emit-bin-op E sp env))
    ((equal? (car E) '+) (emit-bin-op E sp env))
    ((equal? (car E) '-) (emit-bin-op E sp env))
    ((equal? (car E) '/) (emit-bin-op E sp env))
    ((equal? (car E) 'or) (emit-bin-op E sp env))
    ((equal? (car E) 'and) (emit-bin-op E sp env))
    ((equal? (car E) 'var) (emit-var-ref env (cadr E)))
    (else (emit "~a  hello world\n" E))
    )
  )
;---------------------------------------------------------------------------------
; Code generation for program constructs

(define (emit-lambda E env label)
    (emit-label label)
    (extend-global-env label (cadadr E))
    ; fetching arguments from stack
    (emit "move ")
    (emit "move ")
    (driver (cadddr E) sp env)
    (emit "jr $ra")
    )

(define (emit-label label)
  (emit "~a : " label)
  )


(define (emit-imm E sp env)
  (begin
    (emit "li $a0, ~a" (number->string(cadr E)))
    )
  )

(define (emit-define E sp env)
  (extend-global-env (car (cdaddr E)) (cadadr E))
)


(define (emit-cond E sp env)
  (begin
    (let ([true-branch (unique-label)]
          [false-branch (unique-label)]
          [end-if (unique-label)]
          )
      (emit-expr (cadr E) sp env)
      (emit "bne $a0, $0, ~a" true-branch)
      (emit "~a : \n" false-branch)
      (emit-expr (cadddr E) sp env)
      (emit "j ~a" end-if)
      (emit "~a : \n" true-branch)
      (emit-expr (caddr E) sp env)
      (emit "~a :\n" end-if)
      )
    )
  )

(define (emit-display E sp env)
  (cond

    [(equal? 'string (caadr E))
     (emit "li $v0,4")
     (emit "la $a0,~a" (collect-data-section (cadadr E) 0) )
     (emit "syscall")
     ]
    [(equal? 'var (caadr E))
     ;(emit "~a"  (cadadr E))
     (emit-var-ref env  (cadadr E))
     (emit "li	$v0, 1")
     (emit "move $t2, $a0")
     (emit "syscall")
     ]
     [(pair? (cadr E))
       (emit-expr (cadr E) sp env)
       (emit "li	$v0, 1")
       (emit "move $t2, $a0")
       (emit "syscall")
      ; (display "None of these1")
     ]
    [else
      (display "None of these")
      ]
    )
  )

(define (emit-let E sp env)
  (define (sub-emitlet bindings sp new-env)
    (cond
      [(empty? (cadr E)) (emit-expr (cddr E) sp env)]
      [else
        (let ([b (car bindings)])
          (emit-expr (cadr b) sp env)
          (emit-stack-save sp)
          (sub-emitlet (cdr bindings)
                       (next-stack-index sp)
                       (extend-env (cadr b) sp new-env)))])
    )
  (sub-emitlet (cadr E) sp env)
  )

(define (emit-bin-op E sp env)
(emit-expr (cadr E) sp env)
  (emit "sw $a0, 0($sp) \n")
  (emit "addiu $sp, $sp, -4 \n")
  (emit-expr (caddr E) sp env)
  (emit "lw $t1, 4($sp) \n")

  (cond
    ;For plus
    ((equal? (car E) '+)
     (emit "add $a0, $t1, $a0 \n")
     (emit "addiu $sp, $sp, 4 \n")
     )

    ;For minus
    ((equal? (car E) '-)
     (emit "sub $a0, $t1, $a0 \n")
     (emit "addiu $sp, $sp, 4 \n")
     )

    ;For mult
    ((equal? (car E) '*)
     (emit "mult $t1, $a0 \n")
     (emit "sw $a0, $LO \n")
     (emit "addiu $sp, $sp, 4 \n")
     )

    ;For div - for now only integer divisions
    ((equal? (car E) '/)
     (emit "div $t1, $a0 \n")
     (emit "sw $a0, $LO \n")
     (emit "addiu $sp, $sp, 4 \n")
     )

    ;For and - for now only integer divisions
    ((equal? (car E) 'and)
     (emit "and $a0, $t1, $a0 \n")
     (emit "addiu $sp, $sp, 4 \n")

     )

    ;For or - for now only integer divisions
    ((equal? (car E) 'or)
     (emit "or $a0, $t1, $a0 \n")
     (emit "addiu $sp, $sp, 4 \n")
     )

    (else (emit "~a Wrong Operator" E))
    )
  )
; Printing data section
(define collect-data-section
  (let ([data-section '()])
    (lambda (arg1 arg2)
      (cond
        [(equal? arg2 0)

         (let ([lab (symbol->string (unique-label))])
           (set! data-section (append data-section `(,lab)))
           (set! data-section (append data-section '(:.asciiz)))
           (set! data-section (append data-section arg1))
           ;(set! data-section (append data-section '("\n")))
           lab
           )
         ]
        [(equal? arg2 1)


         (cond
           [(null? data-section) '()]
           [else (display ".data\n")(emit-data-section   data-section 0)    ]
           )
         ]
        )
      )
    )
  )

(define (emit-data-section arg arg2)
  (cond
    [(null? arg) '() ]
    [else (cond
            [(equal? arg2 0) (emit "~a ~a \"~a\"" (car arg) (cadr arg) (caddr arg)) (emit-data-section (cdddr arg) 0)]
            )]
    )
  )

;----------------------------TOOLS-------------------------------------

 (define (lookup var env)
   (if (equal? (assv var env) #f)
   (cadr (assv var global-env))
   (cadr (assv var env))
   )
 )
(define (emit-var-ref env vname)
  (cond
    [(emit-stack-load (lookup vname env))]
    [else (display "Error in variable reference" )]
    )
  )

(define (extend-env var sp env)
  (cons (list var sp) env)
  )

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (string->symbol (format "Label_~s" count))])
        (set! count (add1 count))
        L))))

;--------------------------------Code to check token value

(define (let-cg? arg)
  (if (equal? (car arg) 'let)
    (equal? 0 0)
    (equal? 0 1)
    ))

(define (if-cg? arg)
  (if (equal? (car arg) 'if)
    (equal? 0 0)
    (equal? 0 1)
    )
  )

(define (imm-cg? arg)
  (if (equal? (car arg) 'const)
    (equal? 0 0)
    (equal? 0 1)
    )
  )

(define (define-cg? arg)
  (if (equal? (car arg) 'define)
    (equal? 0 0)
    (equal? 0 1)
    )
  )

(define (display-cg? arg)
  (if (equal? (car arg) 'display)
    (equal? 0 0)
    (equal? 0 1)
    )
  )


(define (lambda-cg? arg)
  (if (equal? (car arg) 'display)
    (equal? 0 0)
    (equal? 0 1)
    )
  )

(define (code_gen program)
  (define C (lexParse program))
  (main C)
)
