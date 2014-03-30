;; Simple lexer for the scheme language

;Function to extract string tokens
;;; need to write code for \n \t etc...
;;; Also need to find a function to reverse a list as valstring is evaluated in reverse order.
(define ormap
  (lambda (f . s)
    (letrec ((loop
              (lambda (s)
                (and (pair? (car s))
                     (or (apply f (map car s))
                         (loop (map cdr s)))))))
      (loop s))))

(define (stringfunc listinput valstring)
  (cond
   ((null? listinput) (display "Lexer error, expecting a string end"))
   ((char=? (car listinput) #\") (cons `(string ,(list->string (reverse valstring) ) ) (calc-tokens (cdr listinput))) )
   (else (stringfunc (cdr listinput) (cons (car listinput) valstring)))
   )
)

(define (identifier_token listinput identifier_value)
  ;(display "hello")
    (cond
      ((null? listinput) `((id ,(list->string (reverse identifier_value) ) )))
      ((char-symbol? (car listinput)) (identifier_token (cdr listinput) (cons (car listinput) identifier_value)))
      ((char-delimiter? (car listinput)) (cons `(id ,(list->string (reverse identifier_value) ) )  (calc-tokens listinput)) )
      (else (display "Lexer Error Invalid Indentifier"))
     )
)

(define (comment listinput)
    (cond
      ((null? listinput) '())
      ((char=? (car listinput) #\newline) `(,(calc-tokens(cdr listinput))))
      (else (comment (cdr listinput)))
    )
)
;Start searching for tokens in input string  by looking at each character one by one
  ;Instead of define letrec can be used
(define (calc-tokens listinput)
  (cond
   ((null? listinput) '())  ;if list of characters is null return empty list of tokens
   ((char=? (car listinput) #\() (cons '(leftparen) (calc-tokens (cdr listinput))))   ;left paran
   ((char=? (car listinput) #\)) (cons '(rightparen) (calc-tokens (cdr listinput))))  ;right paran
   ((char=? (car listinput) #\') (cons '(singlequote) (calc-tokens (cdr listinput)))) ;single quote
   ((char=? (car listinput) #\") (stringfunc (cdr listinput) '())) ; Beginning of String , take care of end of string in the corresponding function.
   ((char=? (car listinput) #\;) (comment (cdr listinput) )) ; Beginning of Comment
   ((char=? (car listinput) #\`) (cons '(quasiquote) (calc-tokens (cdr listinput))))  ;quasi quote
   ((char=? (car listinput) #\#) (cons '(hash) (calc-tokens (cdr listinput))))        ;hash
   ((char=? (car listinput) #\,) (cons '(comma) (calc-tokens (cdr listinput))))       ;comma
   ((char=? (car listinput) #\.) (cons '(period) (calc-tokens (cdr listinput))))      ;period
   ((char-whitespace? (car listinput)) (calc-tokens (cdr listinput)))             ;ignore whitespace not make a token out of white space
   ((char=? (car listinput) #\newline) (calc-tokens(cdr listinput)))
   ((char-symbol? (car listinput)) (identifier_token listinput '()))
   (else (display "Lexer Error"))
   )
)

(define check-char-between?
  (lambda (char<=?)
    (lambda (char-from char-to)
      (lambda (char)
        (and (char<=? char-from char)
             (char<=? char char-to))))))

(define char-alphabetic? ((check-char-between? char-ci<=?) #\a #\z))
(define char-decimal? ((check-char-between? char<=?) #\0 #\9))
(define char-symbol?
  (let ((special-chars (string->list "!@$%^*-_=+<>./?:")))
    (lambda (char)
      (or (char-alphabetic? char)
          (char-decimal? char)
          (ormap
           (lambda (special-chars) (char=? special-chars char))
           special-chars)))))

(define char-whitespace?
  (lambda (char)
    (char<=? char #\space)))

(define char-delimiter?
  (lambda (char)
    (or (char-whitespace? char)
        (not (char-symbol? char))
        (null? char)
    )
  )
)

;; Given the input as a string, function returns list of all the token in the string
(define (tokens stringinput)
  (calc-tokens (string->list stringinput))
);; Simple lexer for the scheme language
