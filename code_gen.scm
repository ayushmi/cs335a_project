
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
    ((char=? (car E) #\+)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "add $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")
      ))

    ;For minus
    ((char=? (car E) #\-)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "sub $a0 $t1 $a0 \n")
      (display "addiu $sp $sp 4 \n")
      ))

    ;For mult
    ((char=? (car E) #\*)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "mult $t1 $a0 \n")
      (display "sw $a0 $LO \n")
      (display "addiu $sp $sp 4 \n")
      ))
    
    ;For div - for now only integer divisions
    ((char=? (car E) #\/)      
    (begin 
      (emit-expr (car (cdr E))) 
      (display "sw $a0 0($sp) \n")
      (emit-expr (cdr (cdr E)))
      (display "lw $t1 4($sp) \n")
      (display "div $t1 $a0 \n")
      (display "sw $a0 $LO \n")
      (display "addiu $sp $sp 4 \n")
      ))


    (else (display "Wrong Operator"))
    )
  )



