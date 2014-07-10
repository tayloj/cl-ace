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

(in-package #:cl-ace-util)

;;;; Utilities

(defmacro define-restart-function (name lambda-list &body decls-and-docs)
  "* Syntax:
define-restart-function name lambda-list [[declaration* | documentation]]
* Arguments and Values:
- name --- a symbol, not evaluated
- lambda-list --- a list of symbols
- documentation --- a string, not evaluated
- declaration --- a declare expression, not evaluated
* Description:
define-restart-function defines a function named name whose
lambda-list is like the specified lambda-list, but with one final
optional argument, the condition object.  The function is defined with
DEFUN.  The function searches for a restart named name \(using
FIND-RESTART, name, and the condition object\), and if a restart is
found, it is invoked with INVOKE-RESTART and the variables defined by
the lambda-list \(but not the condition object\)."
  (let ((condition (make-symbol "CONDITION"))
        (restart   (make-symbol "RESTART")))
    `(defun ,name (,@lambda-list &optional ,condition)
       ,@decls-and-docs
       (let ((,restart (find-restart ',name ,condition)))
         (unless (null ,restart)
           (invoke-restart ,restart ,@lambda-list))))))

#+:lispworks
(progn 
  (editor:setup-indent "define-restart-function" 2)
  (dspec:define-form-parser (define-restart-function (:alias defun)))
  (dspec:define-dspec-alias define-restart-function (name)
    `(defun ,name)))

(defun %remf* (plist indicator &rest indicators)
  "Returns a plist like plist but with all properties indicated by
every indicator in indicators removed."
  (dolist (ind (cons indicator indicators) plist)
    (loop while (remf plist ind))))

(define-modify-macro remf* (indicator &rest indicators) %remf*
  "* Syntax:
remf* place indicator+ => plist
* Arguments and Values:
- place --- a place
- indicator --- an object
* Description:
remf* removes from the property list stored in place all properties
with a property indicator identical to any indicator. remf* returns
the property list stored in place.")

(defmacro do-element-nodes ((var node-list-form &optional result-form) &body body)
  "* Syntax:
do-element-nodes (var node-list-form [result-form]) form*
* Arguments and Values:
- var --- a symbol
- node-list-form --- a form
- result-form --- a form
* Description:
do-element-nodes is similar to dom:do-node-list, but body is only
evaluated for a particular node when the node's dom:node-type is
:ELEMENT."
  `(dom:do-node-list (,var ,node-list-form ,result-form)
     (when (eq :element (dom:node-type ,var))
       ,@body)))

(defun map-node-list (fn node-list &aux (results '()))
  "* Syntax:
map-nodes function node-list => results
* Arguments and Values:
- function --- a function of one argument
- node-list --- a dom:node-list
- results --- a list
* Description:
map-nodes is like dom:map-node-list, but returns a list of the results
of calling function on the nodes, so it is more like CL:MAPCAR."
  (dom:do-node-list (n node-list (nreverse results))
    (push (funcall fn n) results)))

(defmacro map-element-nodes (function node-list)
  "* Syntax:
map-element=mpdes function node-list => result-list
* Arguments and Values:
- function --- a function-designator
- node-list --- a node list
- result-list --- a list
* Description:
map-element-nodes returns a list whose elements are the result of
calling function on each element of the node-list \(in order\)."
  (let ((element (make-symbol "ELEMENT"))
        (results (make-symbol "RESULTS"))
        (fn (make-symbol "FUNCTION")))
    `(let ((,results '())
           (,fn ,function))
       (do-element-nodes (,element ,node-list (nreverse ,results))
         (push (funcall ,fn ,element) ,results)))))

(defun text-content (node)
  "* Syntax:
text-content node => content
* Arguments and Values:
- node --- a dom:node
- content --- a string
* Description:
text-content attempts to extract the text content of the node.  If the
node has no child nodes, then content is the zero-length string \"\".
Otherwise, content is the dom:data of node's first child."
  (if (eql 0 (dom:length (dom:child-nodes node))) ""
    (dom:data (dom:first-child node))))

(defmacro pcase (keyform predicate &body body)
  "* Syntax:
- pcase keyform {clause}* => result*
- clause::= (keys form*)
* Arguments and Values:
- keyform --- a form; evaluated to produce a test-key
- predicate --- a form; evaluated to produce a funcallable object
- keys --- a designator for a list of objects. The symbols t and otherwise
may not be used as the keys designator.
- results --- the values returned by the forms in the matching of clause.
* Description:
Pcase is like case, except that the test-key is compared to the keys
by predicate.
* Examples:
;;; (pcase 'foo #'string-equal
;;;   ((bar) 'bar)
;;;   ((\"foo\") 'foo))
;;;  => foo"
  (let ((k (gensym "KEYFORM-"))
        (p (gensym "PREDICATE-")))
    (labels ((expand-clause (clause)
               (destructuring-bind (keys &rest forms) clause
                 `(,(pcase-test keys) ,@forms)))
             (pcase-test (keys)
               (if (or (eq keys 'otherwise) (eq keys 't)) 't
                 (let ((keys (if (listp keys) keys (list keys))))
                   `(or ,@(loop :for key :in keys
                                :collect `(funcall ,p ,k ',key)))))))
      `(let ((,k ,keyform) (,p ,predicate))
         (cond ,@(mapcar #'expand-clause body))))))

(defmacro multiple-value-prog2 (first-form values-form &body forms)
  "* Syntax:
multiple-value-prog2 first-form second-form form* => values
* Arguments and Values:
- form1, values-form, forms --- forms
- values --- values produced by values-form
* Description:
multiple-value-prog2 is to prog2 as multiple-value-prog1 is to prog1."
  `(progn ,first-form (multiple-value-prog1 ,values-form ,@forms)))

(defun make-node-attribute-plist (node &rest args)
  "make-node-attribute-plist node {plist-key specifier}* => plist
* Arguments and Values:
- node --- a Closure XML dom:node
- plist-key --- a symbol
- specifier ::= attribute-name | (attribute-name &key key optional)
- attribute-name --- a string designator
- key --- a function or symbol
- optional --- a generalized boolean
* Description:
Make-node-attribute-plist builds a plist whose keys are the plist-keys
and whose values are based the corresponding specifier and XML
attribute values of the node. Values are computed as follows:
- In the case that specifier is simply attribute-name, i.e., a string
designator, then the value of the corresponding plist-key is the value
of the XML attribute with the name designated by attribute-name.
- In the case that specifier is a list, then key is applied to the XML
attribute value and the result is used as the value in the resulting
plist. If optional is non-nil, then if the attribute is not specified
for the DOM node, the the plist-key is skipped.
* Examples:
Where node is a dom:node representing <element att1=\"val1\" />:
;;;  (make-node-attribute-plist node :foo \"att1\")
;;;    => (:foo \"att1\")
;;; (make-node-attribute-plist node :foo '(\"att1\" :key reverse))
;;;    => (:foo \"1tta\")
;;; (make-node-attribute-plist node :foo '(\"att2\" :key intern))
;;;    => (:foo ||)
;;; (make-node-attribute-plist node :foo '(\"att2\" :key intern :optional t))
;;;    => ()"
  (do* ((plist '())
        (args args (cddr args))
        (key (first args) (first args))
        (value (second args) (second args)))
       ((endp args) plist)
    (destructuring-bind (name &key optional ((:key process) 'identity))
        (if (consp value) value (list value))
      (unless (and optional (not (dom:has-attribute node name)))
        (let* ((name (string name))
               (value (dom:get-attribute node name))
               (value (funcall process value)))
          (setf plist (list* key value plist)))))))

(defun make-node-instance (class node &rest initargs)
  "* Syntax:
make-node-instance class node &rest initargs => object
* Arguments and Values: 
- class --- a class, or symbol that names a class
- node --- a cxml dom:node
- initargs --- a plist
- object --- an object of type class
* Description:
Make-node-instance is a simple wrapper that applies make-instance to
class and initialization arguments that are generated by
make-node-attribute-plist from node and initargs."
  (apply 'make-instance class
         (apply 'make-node-attribute-plist node initargs)))

#+:lispworks
(editor:setup-indent "make-node-instance" 2)

(defun keywordify (string)
  "keywordify string => keyword
* Arguments and Values:
- string --- a string designator
- keyword --- a keyword
* Description: 
Keywordify returns a keyword whose symbol name is the result of
replacing #\\_ with #\\- in the string-upcase'd string.
* Examples:
;;; (keywordify \"comp_than\") => :comp-than"
  (intern (substitute #\- #\_ (string-upcase string)) :keyword))

(defun read-safely-from-string (string
                                &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  "* Syntax:
read-safely-from-string string
  &optional eof-error-p t eof-value
  &key start end preserve-whitespace
  => object, position
* Arguments and Values:
Arguments and Values are exactly those of READ-FROM-STRING.
* Description:
read-safely-from-string binds *READ-EVAL* to NIL, and calls
READ-FROM-STRING with its arguments."
  (declare (string string))
  (let ((*read-eval* nil))
    (read-from-string
     string eof-error-p eof-value
     :start start :end end
     :preserve-whitespace preserve-whitespace)))

(defun parse-type (type string &key (start 0) end)
  "* Syntax:
parse-type type string &key start end => value
* Arguments and Values:
- type --- a type specifier
- string --- a string
- start, end --- bounding index designators of string. Defaults are 0
and nil, respectively.
- float --- the parsed value
* Description:
Reads a value from string using read-safely-from-string. If the value
satifies the type specifier, it is returned. Otherwise an error is
signalled."
  (declare (string string))
  (let ((value (read-safely-from-string string :start start :end end)))
    (if (typep value type) value
      (error "No ~A present in ~S from ~S to ~S."
             type string start (if end end (length string))))))

(defun parse-float (string &key (start 0) end)
  "* Syntax:
parse-float string &key start end => float
* Arguments and Values:
- string --- a string
- start, end --- bounding index designators of string. Defaults are 0
and nil, respectively.
- float --- the parsed float
* Description:
Equivalent to (parse-type 'float string :start start :end end)."
  (parse-type 'float string :start start :end end))

(defun parse-real (string &key (start 0) end)
  "* Syntax:
parse-real string &key start end => real
* Arguments and Values:
- string --- a string
- start, end --- bounding index designators of string. Defaults are 0
and nil, respectively
- real --- the parsed real
* Description:
Equivalent to (parse-type 'real string :start start :end end)."
  (parse-type 'real string :start start :end end))

(defun strcat (&rest string-designators)
  "* Syntax:
strcat &rest string-designators => string
* Argument and Values:
- string-designators --- string designators
- string --- a string
* Description:
strcat returns a string which is the concatenation of the designated
strings."
  (with-output-to-string (out)
    (dolist (string string-designators)
      (write-string (string string) out))))

(defun split-sequence (separators sequence &key (test 'eql) key coalesce-separators)
  "* Syntax:
split-sequence separators sequence &key key test coalesce-separators => result
* Arguments and Values:
- separators --- a sequence of separators
- sequence --- a sequent to split
- key --- a function of one argument or nil
- test --- a function of two arguments, default is eql
- coalesce-separators --- a boolean
* Description:
split-sequence returns a list of sequences of the same type as
sequences \(extracted using subseq\) where the subsequences are
contiguous subsequences of sequence, delimited by any element of
separators. Separators are found within sequence using test and key.
If coalesce-separators is non-nil, then separators within sequence are
merged \(i.e., result will contain no empty sequences\).
* Examples:
;;;  (split-sequence #(a) '(t i a r a))
;;; => ((T I) (R) NIL)
;;; (split-sequence #(a) '(t i a r a) :coalesce-separators t)
;;; => ((T I) (R))
" 
  (flet ((separatorp (object)
           (position object separators :test test :key key)))
    (do* ((left 0 (1+ (the fixnum right)))
          (right (position-if #'separatorp sequence :start left)
                 (position-if #'separatorp sequence :start left))
          (match (subseq sequence left right)
                 (subseq sequence left right))
          (elements (list match)
                    (if (and coalesce-separators
                             (every #'separatorp match))
                      elements
                      (cons match elements))))
         ((null right) (nreverse elements))
      (declare (fixnum left)))))

(defun prompt-for-line (message &rest args)
  "* Syntax:
prompt-for-line message &rest args => line, okp
* Arguments and Values:
- message --- a format control
- args --- arguments used by format
- line --- a string
- okp --- a boolean
* Description:
Prompts for a line of input on *query-io* after presenting the message
using the args.  A line is read from *query-io*.  If the call to
read-line returns without an error, the line that was read is
returned, and okp is true.  Otherwise the empty string is returned,
and okp is false."
  (fresh-line *query-io*)
  (apply 'format *query-io* message args)
  (write-char #\space *query-io*)
  (let ((line (read-line *query-io* nil nil)))
    (if (null line)
      (values "" nil)
      (values line t))))

(defmacro with-namespaces (bindings &body body)
  "* Syntax:
with-namespaces ({(prefix uri)}*) form* => result
* Arguments and Values:
- prefix --- a namespace prefix \(a string\)
- uri --- a string denoting a URI
- form --- a form
- result --- the result of evaluating forms
* Description:
with-namespaces evalutes forms with the the namespace bindings
provided.  I.e., it reduces nesting of CXML:WITH-NAMESPACE forms."
  (if (endp bindings) `(progn ,@body)
    (destructuring-bind ((prefix uri) &rest bindings) bindings
      (if (endp bindings)
        `(cxml:with-namespace (,prefix ,uri)
           ,@body)
        `(cxml:with-namespace (,prefix ,uri)
           (with-namespaces ,bindings
             ,@body))))))

(defmacro with-soap-envelope ((prefix &rest bindings) &body body)
  "* Syntax:
with-soap-envelope (soap-prefix {(prefix uri)}*) form* => envelope
* Arguments and Values:
- soap-prefix, prefix --- namespace abbreviations/prefixes
- uri --- string denoting a URI
- forms --- forms
- envelope --- a CXML:ROD (probably a string)
* Description:
with-soap-envelope evalutes forms within a CXML:WITH-XML-OUTPUT with
the following XML namespaces.  prefix is bound to the SOAP envelope
prefix, i.e., \"http://schemas.xmlsoap.org/soap/envelope/\".  Any
other namespace bindings provided are also in effect.  Forms are
evaluated within two CXML:WITH-ELEMENTs, the SOAP Envelope and the
SOAP Body.  A CXML:ROD \(probably a string\) is returned.
* Examples:
;;; (with-soap-envelope
;;;     (\"env\" (\"foo\" \"http://foo.example.org\")
;;;            (\"bar\" \"http://bar.example.org\"))
;;;   (cxml:with-element* (\"foo\" \"a\")
;;;     (cxml:with-element* (\"bar\" \"b\")
;;;       (cxml:text \"Text Content\"))))
;;; => 
;;; \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
;;;  <env:Envelope xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"
;;;                xmlns:foo=\"http://foo.example.org\"
;;;                xmlns:bar=\"http://bar.example.org\">
;;;    <env:Body>
;;;      <foo:a>
;;;        <bar:b>Text Content</bar:b>
;;;      </foo:a>
;;;    </env:Body>
;;;  </env:Envelope>\"
"
  (let ((pre (gensym (string '#:prefix-))))
    `(let ((,pre ,prefix))
       (cxml:with-xml-output (cxml:make-rod-sink)
         (cxml:with-namespace (,pre "http://schemas.xmlsoap.org/soap/envelope/")
           (with-namespaces ,bindings 
             (cxml:with-element* (,pre "Envelope")
               (cxml:with-element* (,pre "Body")
                 ,@body))))))))
