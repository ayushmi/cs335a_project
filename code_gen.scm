(load "tests-driver.scm")

;----------------------------------------------------------------------------------
; #defines
(define wordsize 4)

;----------------------------------------------------------------------------------
; Stack operations
(define (emit-stack-load sp)
  (emit " move ~s($sp), $a0" sp)
  )

(define (next-stack-index sp)
  (- sp wordsize)
  )
;---------------------------------------------------------------------------------
; Main Function
(define (main E)
  (define sp 100)
  (define env '())
  (emit ".text")
  (driver E sp env)
  (collect-data-section '() 1)
)

;---------------------------DRIVER--------------------------------
(define (driver E sp env)
  (cond
    [(null? E) '()]
    [(null? (cdr E)) (emit-expr (car E) sp env)]
    [else (emit-expr (car E) sp env) (driver (cdr E))]
    )
)

;---------------------------------------------------------------------------------
; Top level function    EMIT-EXPR
(define (emit-expr E sp env)
  (cond
    ((imm? E) (emit-imm E sp env))
    ((if? E)  (emit-cond E sp env))
    ((let? E) (emit-let E sp env))
    ((define? E) (emit-define E sp env))
    ((display? E) (emit-display E sp env))
    ((equal? (car E) #\*) (emit-bin-op E sp env))
    ((equal? (car E) #\+) (emit-bin-op E sp env))
    ((equal? (car E) #\-) (emit-bin-op E sp env))
    ((equal? (car E) #\/) (emit-bin-op E sp env))
    ((equal? (car E) 'or) (emit-bin-op E sp env))
    ((equal? (car E) 'and) (emit-bin-op E sp env))
    (else (display "hello world\n"))
    )
  )
;---------------------------------------------------------------------------------
; Code generation for program constructs
(define (emit-imm E sp env)
  (begin
    (emit "li $a0 ~a" (number->string(cadr E)))
  )
)

(define (emit-define E sp env)
  (display "asd")
  )

(define (emit-cond E sp env)
  (begin
    (let ([true-branch (unique-label)]
          [false-branch (unique-label)]
          [end-if (unique-label)]
          )
      (emit-expr (car (cadr E)))
      (emit "beq $a0 $0 ~a" true-branch)
      (emit "~a : \n" false-branch)
      (emit-expr (caddr (cadr E)))
      (emit "j ~a" end-if)
      (emit "~a : \n" true-branch)
      (emit-expr (cadr (cadr E)))
      (emit "~a :\n" end-if)
      )
    )
  )

(define (emit-display E sp env)
  (cond
    [(equal? 'string (caadr E))
     (emit "li $v0,4")
     (emit "la $a0,~a" (collect-data-section (cadr E) 0) )
     (emit "syscall")
    ]
    [(equal? 'var (caadr E))
		(emit-var-ref env vname)
		(emit "li	$v0, 1")
		(emit "move $a0, $t2")
		(emit "syscall")
    ]
    [else
      (display "None of these")
     ]
   )
)

(define (emit-let E sp env)
  (define (sub-emitlet bindings sp new-env)
    (cond
      [(empty? (cadr E)) (emit-expr (cddr E))]
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
    (emit-expr (car (cdr E)))
    (emit "sw $a0 0($sp) \n")
    (emit "addiu $sp $sp -4 \n")
    (emit-expr (cdr (cdr E)))
    (emit "lw $t1 4($sp) \n")

  (cond
    ;For plus
    ((equal? (car E) #\+)
        (emit "add $a0 $t1 $a0 \n")
        (emit "addiu $sp $sp 4 \n")
       )

    ;For minus
    ((equal? (car E) #\-)
       (emit "sub $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")
       )

    ;For mult
    ((equal? (car E) #\*)
       (emit "mult $t1 $a0 \n")
       (emit "sw $a0 $LO \n")
       (emit "addiu $sp $sp 4 \n")
       )

    ;For div - for now only integer divisions
    ((equal? (car E) #\/)
       (emit "div $t1 $a0 \n")
       (emit "sw $a0 $LO \n")
       (emit "addiu $sp $sp 4 \n")
       )

    ;For and - for now only integer divisions
    ((equal? (car E) 'and)
       (emit "and $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")

       )

    ;For or - for now only integer divisions
    ((equal? (car E) 'or)
       (emit "or $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")
       )

    (else (emit "Wrong Operator"))
    )
  )
; Printing data section
(define collect-data-section
  (let ([data-section '()])
    (lambda (arg1 arg2)
      (cond
        [(equal? arg2 0)
          (display "0----")
         (let ([lab (symbol->string (unique-label))])
           (set! data-section (append data-section `(,lab)))
           (set! data-section (append data-section `(,arg1)))
           (set! data-section (append data-section '("\n")))
           lab
           )
         ]
        [(equal? arg2 1)

          (display "1----")
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

            [(equal? arg2 0) (emit ".asciiz    \"~a\" " (car arg)) (emit-data-section (cdr arg) 1)]
            [(equal? arg2 1) (emit "\"~a\" " (car arg)) (emit-data-section (cdr arg) 1)]
            )]
    )
  )




;----------------------------TOOLS-------------------------------------

(define (lookup var env)
  (cdr (assv var env))
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

(define (let? arg)
  (if (equal? (car arg) 'let)
    (equal? 0 0)
    (equal? 0 1)
    ))

(define (if? arg)
  (if (equal? (car arg) 'if)
    (equal? 0 0)
    (equal? 0 1)
    )
  )

(define (imm? arg)
  (if (equal? (car arg) 'var)
    (equal? 0 0)
    (equal? 0 1)
    )
  )
(define (define? arg)
  (if (equal? (car arg) 'define)
    (equal? 0 0)
    (equal? 0 1)
    ))

(define (display? arg)
  (if (equal? (car arg) 'display)
    (equal? 0 0)
    (equal? 0 1)
    )
  )
