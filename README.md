cs335a_project
==============

In this project we are building a compiler for a subset of scheme.

The target language is scheme and the implementation language is scheme.

lexer.scm is the lexer code which defines 'tokens' function, 'tokens' takes the program as input string and outputs the stream of tokens in form of a list.

parser.scm is the parser file which defines 'parser' function , 'parser' takes the list containing tokens as input and outputs the parse tree in form of recursive lists.

code_gen.scm is the code generation file which generates mips code for the input parsed code.



test#1
(display (+ 1 -2))

test#6
(if 1 (begin (define x 2) (display x))) (if 1 (begin (define x 4) (display x)))

test#5
(define x 15)
(if 
	(> x 5) 
	(
		if (< x 10) 
		(display \"between 5 and 10\") 
		(display \">10\")
	) 
	(display \"<5\")
)






Specification for the subset of Scheme implemented.
1. Type - Integers and strings
2. + - * / = and or
3. if statements
4. display - int and string and any general expression
5. Global Variables
6. Lexical Scoping
7. Globally defined variables are at lesser priority than locally scoped variables of the same name.
8. let is converted to corresponding lambda function call.
9. Functions
10. Recursion

Test Cases
1.
(define x 5)
(display (+ (+ 1 2) 3))
(display (+ (+ x 5) 3))
(display (if (and 1 0) x (+ x 1)))
(if (or 1 0) x (+ x 1))

2.
(define (f n) (if (- n 0) 0  (+ 1 (f (- n 1)) )))
(f 5)

3.
(let ((x 1) (y 2))
  (+ x y))

4.
(define (add x y) ((+ x y))) (add 4 5)
