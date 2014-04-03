
(define (emit-expr E)
  (begin 
   (display "li $a0 ")
   (display (car E))
   (display "\n")
   )
)

(define (emit-bin-op E)
  (cond


    ;For plus
    ((equal? (car E) #\+)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")  
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "add $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")
      ))

    ;For minus
    ((equal? (car E) #\-)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "sub $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")
      ))

    ;For mult
    ((equal? (car E) #\*)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "mult $t1 $a0 \n")
      (display "sw $a0 $LO \n")
      (display "addiu $sp $sp 4 \n")
      ))
    
    ;For div - for now only integer divisions
    ((equal? (car E) #\/)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "div $t1 $a0 \n")
      (display "sw $a0 $LO \n")
      (display "addiu $sp $sp 4 \n")
      ))

    ;For and - for now only integer divisions
    ((equal? (car E) 'and)      
    (begin 
      
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "and $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")

      ))
    
    ;For or - for now only integer divisions
    ((equal? (car E) 'or)      
    (begin
      
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (display "addiu $sp $sp -4 \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "or $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")

      ))


    (else (display "Wrong Operator"))
    )
  )



