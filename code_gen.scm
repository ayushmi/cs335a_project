(load "tests-driver.scm")



(define (let? arg)
  (if (equal? (car arg) "let")
    (equal? 0 0)
    (equal? 0 1)
    ))
(define (if? arg)
  (if (equal? (car arg) "if")
    (equal? 0 0)
    (equal? 0 1)
    )
  )
(define (imm? arg)
  (if (equal? (car arg) "id")
    (equal? 0 0)
    (equal? 0 1)
    )
)
(define (define? arg)
  (if (equal? (car arg) "define")
    (equal? 0 0)
    (equal? 0 1)
    ))

(define wordsize 4)

(define (emit-stack-load sp)
  (emit " move ~s($sp), $a0" sp))

(define (next-stack-index sp)
  (- sp wordsize)
  )
(define (emit-expr E sp env)
  (cond
    ((imm? E) (emit-imm E sp env))
    ((if? E) (emit-cond E sp env))
    ((let? E) (emit-let E sp env))
    ((define? E) (emit-define E sp env))
    ((equal? (car E) #\*) (emit-bin-op E sp env))
    ((equal? (car E) #\+) (emit-bin-op E sp env))
    ((equal? (car E) #\-) (emit-bin-op E sp env))
    ((equal? (car E) #\/) (emit-bin-op E sp env))
    ((equal? (car E) 'or) (emit-bin-op E sp env))
    ((equal? (car E) 'and) (emit-bin-op E sp env))
	(else (display "hello world\n"))
   )
  )

(define (emit-imm E sp env)
  (begin
    (emit "li $a0 ~a" (number->string(cadr E)))
    )
  )
(define (emit-define E sp env)
  (display "asd")
  )

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (string->symbol (format "Label_~s" count))])
        (set! count (add1 count))
        L))))

(define (emit-cond E sp env)
  (begin
    (let ([true-branch (unique-label)]
          [false-branch (unique-label)]
          [end-if (unique-label)]
          )
      (emit-expr (car (cdr E)))
      (emit "beq $a0 $0 ~a" true-branch)
      (emit "~a : \n" false-branch)
      (emit-expr (car (cdr (cdr (cdr  E)))))
      (emit "j ~a" end-if)
      (emit "~a : \n" true-branch)
      (emit-expr (car (cdr (cdr E))))
      (emit "~a :\n" end-if)
      )
    )
  )

(define (extend-env var sp env)
    (cons (list var sp) env))

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
 (sub-emitlet((cadr E) sp env))
)

(define (lookup var env)
  (cdr (assv var env))
)
(define (emit-var-ref env vname)
  (cond
    [(emit-stack-load (lookup vname env))]
    [else (display "display Error-->DHRUV" )]
    )
  )
(define (emit-bin-op E sp env)
  (cond
    ;For plus
    ((equal? (car E) #\+)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "add $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")
       ))

    ;For minus
    ((equal? (car E) #\-)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "sub $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")
       ))

    ;For mult
    ((equal? (car E) #\*)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "mult $t1 $a0 \n")
       (emit "sw $a0 $LO \n")
       (emit "addiu $sp $sp 4 \n")
       ))

    ;For div - for now only integer divisions
    ((equal? (car E) #\/)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "div $t1 $a0 \n")
       (emit "sw $a0 $LO \n")
       (emit "addiu $sp $sp 4 \n")
       ))

    ;For and - for now only integer divisions
    ((equal? (car E) 'and)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "and $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")

       ))

    ;For or - for now only integer divisions
    ((equal? (car E) 'or)
     (begin
       (emit-expr (car (cdr E)))
       (emit "sw $a0 0($sp) \n")
       (emit "addiu $sp $sp -4 \n")
       (emit-expr (cdr (cdr E)))
       (emit "lw $t1 4($sp) \n")
       (emit "or $a0 $t1 $a0 \n")
       (emit "addiu $sp $sp 4 \n")
       )
     )
    (else (emit "Wrong Operator"))
    )
  )
