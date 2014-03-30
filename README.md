cs335a_project
==============

In this project we are building a compiler for a subset of scheme.

The target language is scheme and the implementation language is scheme.

lexer.scm is the lexer code which defines 'tokens' function, 'tokens' takes the program as input string and outputs the stream of tokens in form of a list.

parser.scm is the parser file which defines 'parser' function , 'parser' takes the list containing tokens as input and outputs the parse tree in form of recursive lists.

