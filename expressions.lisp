;;; Copyright (c) 2009, Joshua Taylor.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:ace-drs)

;;;; Parsing ACE Expressions

;;; The arithmetic (and string) expressions allowed by ACE are output
;;; in an easily parseable format. It's simple enough, in fact, that
;;; we can build a recursive descent parser with no backtracking. The
;;; only non-straightforward part is that inside string and named
;;; expressions, the contents can be quoted or non-quoted, so there is
;;; a string extraction function. The grammar is as follows:
;;;
;;; <EXPR> ::= NAMED ( string ) | INT ( string )
;;;          | REAL ( string ) | STRING ( string )
;;;          | EXPR ( { + | - | & | * | / } , <EXPR> , <EXPR> )
;;;          | LIST ( [ <EXPR> { , <EXPR> }* ] )
;;;          | SET ( [ <EXPR> { , <EXPR> }* ] )
;;;
;;; where <EXPR> is the only nonterminal, and string is the only token
;;; with an associated value. In fact, in named and string
;;; expressions, string may be a quoted or non-quoted string. (Non
;;; quoted strings are terminated by EOF or an ACE special character.
;;; Quoted strings begin with #\' and end with an unescaped #\'.) For
;;; int and real (EXPR) expressions, string must be unquoted.

;;; Tokenizing

(defun special-ace-char-p (character)
  "The characters in \",()[]{}+-&*/\" are special within the context
of ACE expression output."
  (case character
    (#.(coerce ",()[]{}+-&*/" 'list) t)
    (otherwise nil)))

(defun special-ace-char-token (character)
  (ecase character
    ((#\,) :comma)
    ((#\() :lparen)
    ((#\)) :rparen)
    ((#\[) :lbracket)
    ((#\]) :rbracket)
    ((#\{) :lbrace)
    ((#\}) :rbrace)
    ((#\+) :plus)
    ((#\-) :minus)
    ((#\&) :concatenate)
    ((#\*) :times)
    ((#\/) :divide)))

(defun whitespace-char-p (character)
  "True of exactly #\Space, #\Tab, #\Return, and #\Newline."
  ;; I'm not sure exactly what characters we could see, but this is a
  ;; fairly standard set that seems like a safe bet.
  (case character
    ((#\space #\tab #\return #\newline) t)
    (otherwise nil)))

(defun consume-whitespace (&optional (input *standard-input*))
  "Consume characters satisfying whitespace-char-p form input."
  (do () ((not (whitespace-char-p (peek-char nil input nil #\a))))
    (read-char input)))

(defun read-token (&optional (input *standard-input*) (eof-error-p t) eof-value)
  "* Syntax:
read-token input &optional eof-error-p eof-value => results*
* Arguments and Values:
- input --- an input stream
- eof-error-p --- a generalized boolean, default is t
- eof-value --- an object, default is nil
- results --- see below
* Description:
Read-token reads a token from input. eof-error-p and eof-value are as
to READ, &c.  In the case that token can be read, the token is
returned as the primary value, and in case the token is :TEXT, the
text value is returned as a secondary value."
  (consume-whitespace input)
  (let ((c (read-char input nil nil)))
    (if (null c)
      (if (not eof-error-p) eof-value
        (error "EOF encountered."))
      (if (special-ace-char-p c) (special-ace-char-token c)
        (let ((string (with-output-to-string (*standard-output*)
                        (loop :initially (write-char c)
                              :for x := (peek-char nil input nil nil nil)
                              :until (or (null x) (special-ace-char-p x))
                              :do (write-char (read-char input))))))
          (pcase string 'string=
            (("named") :named)
            (("int") :int)
            (("real") :real)
            (("string") :string)
            (("expr") :expr)
            (("list") :list)
            (("set") :set)
            (otherwise (values :text string))))))))

(defun consume-token (token &optional (input *standard-input*))
  "* Syntax:
consume-token token &optional input => token, value
* Arguments and Values:
- token --- a token
- input --- an input stream, default is *standard-input*
* Description:
Reads a token from input using read-token, asserts that it is in fact
token. Returns the token and associated value."
  (multiple-value-bind (rtoken value) (read-token input nil nil)
    (assert (eq token rtoken) () "Expected ~S, found ~S." token rtoken)
    (values token value)))

(defun invoke-with-token-value (token input function)
  "Consume token via consume-token on input and call function with the
associated value."
  (funcall function (nth-value 1 (consume-token token input))))

(defmacro with-token-value ((value token input) &body body)
  "A macro around invoke-with-token-value. Consumes token from input,
binds value to the associated value, and evalautes body within the
binding, returning values produced by body."
  `(invoke-with-token-value
    ,token ,input #'(lambda (,value) ,@body)))

;;; Reading Expressions

(defun read-expression (&optional (input *standard-input*)
                                  (no-expression-error-p t)
                                  (eof-error-p t) eof-value)
  "* Syntax:
read-expression &optional input no-expression-error-p eof-error-p eof-value
  => result*
* Arguments and Values:
- input --- an input stream
- no-expression-error-p, eof-error-p --- generalized booleans, default is t
- eof-value --- an object, default is nil
- results --- an expression, eof-value, or a token and value as
primary and secondary values
* Description:
read-expression attempts to read an expression from the input.  If an
expression can be read, it is returned. If an expression cannot be
read, it is either because an EOF was envountered, or because a token
was read but was not one beginning an expression.  If and EOF was
enountered, behavior is detemined by eof-error-p and eof-value, as to
READ, &c.  If tokens are available, but do not begin an expression,
behavior is determined by no-error-expression-p. If this value is
true, then an error is signalled. Otherwise, the token and its
associated value are returned as primary and secondary values.
* Examples:
Reading a named expression:
;;; (read-expression)
;;; >> named('John')
;;; => #<named(John)>
Reading a compound expression:
;;; (ace-drs:read-expression)
;;; >> expr(&,string(hello),string(' world!'))
;;; => #<expr(CONCATENATE,#<string(hello)>,#<string( world!)>)>"
  (multiple-value-bind (token value) (read-token input nil nil)
    (case token
      ;; EOF
      ((nil) (if (not eof-error-p) eof-value
               (error "EOF encountered.")))
      ;; Start reading an expression
      ((:named) (read-named-expression input))
      ((:int) (read-int-expression input))
      ((:real) (read-real-expression input))
      ((:string) (read-string-expression input))
      ((:expr) (read-compound-expression input))
      ((:list) (read-list-expression input))
      ((:set) (read-set-expression input))
      ;; Non-expression beginning token
      (otherwise (if (not no-expression-error-p) (values token value)
                   (error "Malformed expression."))))))

(defun read-expression-from-string (string
                                    &optional
                                    (no-expression-error-p t)
                                    (eof-error-p t) eof-value
                                    &key (start 0) end)
  "* Syntax:
read-expression-from-string
  string &optional no-expression-error-p eof-error-p eof-value &key start end
  => result*
* Arguments and Values:
- string --- a string
- no-expression-error-p, eof-error-p, eof-value --- as to read-expression
- start, end --- bounding index designators for string
* Description:
read-expression-from-string reads an expression from string by
creating an input string stream (via WITH-INPUT-FROM-STRING and
passing it to {defun ace-drs::read-expression} .
* Examples:
;;; (ace-drs:read-expression-from-string
;;;  \"medalist([int(1),real(2)])\" t t nil :start 4)
;;; => #<list([#<int(1)>,#<string(three)>])>"
  (with-input-from-string (in string :start start :end end)
    (read-expression in no-expression-error-p eof-error-p eof-value)))

;;; Auxiliary reading functions

(defun read-string (input)
  "Read and return string content from input. If the first character
available from input is #\\', then the string is understood to be
quoted, within characters may be escaped by #\\\, and is ended by the
next non-escaped #\'. Otherwise, the string content is not quoted,
charactes are not escaped, and the string is terminated by the next
#\) on the stream."
  (with-output-to-string (*standard-output*)
    (let ((c (read-char input)))
      (declare (character c))
      (if (char= c #\')
        ;; quoted string, terminated by #\' within
        (loop (setf c (read-char input))
              (if (char= #\' c) (return)
                (if (char/= #\\ c) (write-char c)
                  (write-char (read-char input)))))
        ;; unquoted string, terminated by #\) without
        (loop (write-char c) (setf c (peek-char nil input nil #\a))
              (if (char= #\) c) (return)
                (setf c (read-char input))))))))

(defun invoke-consuming-delimiters (left right function input)
  "Invokes function between consuming the tokens left and right from
input.  Returns any values produced by function."
  (multiple-value-prog2 (consume-token left) (funcall function)
    (consume-token right input)))

(defmacro within-parens (input &body body)
  "Wrapper for invoke-consuming-delimiters that consume :LPAREN and
:RPAREN from input around body. Any values from body are returned."
  `(invoke-consuming-delimiters
    :lparen :rparen #'(lambda () ,@body) ,input))

(defun read-delimited-expression-list (delimiter input)
  "Read a comma separated list of expressions, terminated by
delimiter. Delimiter is consumed. Returns the list of expressions."
  (let ((e/t (read-expression input nil t)))
    (if (typep e/t 'expression)
      ;; if an expression, save it and collect the rest of the list
      (do ((elements (list e/t) (cons (read-expression input) elements))
           (token (read-token input) (read-token input)))
          ((eq token delimiter) (nreverse elements))
        (assert (eq token :comma) ()
          "Expected #\, between expressions., got ~S." token))
      ;; otherwise, ensure that it's the end deliminter (and return '())
      (assert (eq e/t delimiter) ()
        "Expression list ended by ~S, not ~S." e/t delimiter))))

(defun read-prolog-expression-list (input)
  "Read and return a list delimited by :LBRACKET and :RBRACKET tokens,
i.e., the characters #\\[ and #\\]."
  (consume-token :lbracket input)
  (read-delimited-expression-list :rbracket input))

;;; Reading various types of expressions

;;; In all of these function, the initial portion of the expression
;;; has already been consumed. E.g., though a named expression is
;;; named(<string>), and the corresponding tokens are :NAMED :LPAREN
;;; ... :RPAREN, the :NAMED token has already been consumed by
;;; read-expression.

(defun read-named-expression (input)
  (within-parens input
    (make-instance
     'named-expression
     :name (read-string input))))

(defun read-int-expression (input)
  (within-parens input
    (with-token-value (value :text input)
      (make-instance
       'int-expression
       :integer (parse-integer value)))))

(defun read-real-expression (input)
  (within-parens input
    (with-token-value (value :text input)
      (make-instance
       'real-expression
       :real (parse-real value)))))

(defun read-string-expression (input)
  (within-parens input
    (make-instance
     'string-expression
     :string (read-string input))))

(defun read-compound-expression (input)
  (consume-token :lparen input)
  (let ((operator (read-token input)))
    (consume-token :comma input)
    (make-instance
     'compound-expression
     :operator operator
     :arguments (read-delimited-expression-list :rparen input))))

(defun read-list-expression (input)
  (within-parens input
    (make-instance
     'list-expression
     :elements (read-prolog-expression-list input))))

(defun read-set-expression (input)
  (within-parens input
    (make-instance
     'set-expression
     :elements (read-prolog-expression-list input))))
