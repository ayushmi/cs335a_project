.text
main:
li $v0,4
la $a0,Label_0
syscall
li $v0,10

syscall

.data
Label_0 :.asciiz "Hello World!"