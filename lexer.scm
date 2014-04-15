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

(define (give-symbol-name sym)
  (cond
    ((string->number sym) `(number, (string->number sym)))
    ((equal? sym "define") '(symbol define))
    ((equal? sym "if") '(symbol if))
    ((equal? sym "lambda") '(symbol lambda))
    ((equal? sym "or") '(symbol or))
    (else `(symbol, (string->symbol sym)))
  )
)

(define (identifier_token listinput identifier_value)
  ;(display "hello")
    (cond
      ((null? listinput) `((id ,(list->string (reverse identifier_value) ) )))
      ((char-symbol? (car listinput)) (identifier_token (cdr listinput) (cons (car listinput) identifier_value)))
    ((char-delimiter? (car listinput)) (cons (give-symbol-name (list->string (reverse identifier_value)))  (calc-tokens listinput)) )
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

(define (escapefunc)
            (lambda (s)
              (cond ((null? s) (error 'scanner "Must be one char after #\\"))
              (else (charfunc (cdr s) (list (car s)))))))

(define (charfunc)
  (lambda (s chars)
    (cond ((null? s) `((char ,(make-char chars)) ,@(st-init '())))
          ((char-delimiter? (car s))
           `((char ,(make-char chars)) ,@(st-init s)))
         (else (charfunc (cdr s) (cons (car s) chars))))))

(define (hashfunc s)
    (cond
      ((null? s) (display "Expecting something after #, but reached end"))
      ((char=? (car s) #\\) (escapefunc (cdr s)))
      ((char-ci=? (car s) #\f) `((boolean #f) ,@(calc-tokens (cdr s))))
      ((char-ci=? (car s) #\t) `((boolean #t) ,@(calc-tokens (cdr s))))
      ((char=? (car s) #\;) `((comment) ,@(comment (cdr s))))
      (else (display
             "Expecting t, f, \\, ( after #, but found" s))))

(define (make-char)
  (lambda (chars)
    (cond ((null? chars) (display "Found #\\ without any char"))
          ((null? (cdr chars)) (car chars))
          (else (let* ((string (list->string (reverse chars)))
                       (maybe-number (string->number string 8)))
                  (if (number? maybe-number)
                      (integer->char maybe-number)
                      (cond ((string-ci=? string "return") #\return)
                            ((string-ci=? string "newline") #\newline)
                            ((string-ci=? string "space") #\space)
                            ((string-ci=? string "tab") #\tab)
                            ((string-ci=? string "page") #\page)
                            (else (display
                                   "Can't recognize the following character: "
                                   (format "#\\~s" string))))))))))

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
   ((char=? (car listinput) #\#) (hashfunc (cdr listinput)))        ;hash
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
