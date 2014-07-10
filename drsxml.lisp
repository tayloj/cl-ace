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

;;;; Parsing DRS from DRSXML

;;; Referents

(defvar *referents* '()
  "A list of DRS domains (lists) in scope while parsing a DRS. Earlier
elements denote more closely bound referents.")

(defun resolve-referent (name &optional (referents *referents*))
  "* Syntax:
resolve-referent name [referents] => result
* Arguments and Values:
- name --- a string
- referents --- a list of lists of referents, defaults to {defvar *referents*}
- result --- a referent, or nil
* Description:
Returns the most recently bound referent with the given name, or name
if there is no such referent."
  (dolist (scope referents name)
    (let ((referent (find name scope :key 'referent-name :test 'string=)))
      (when (not (null referent))
        (return referent)))))

;;; Referent or Expression

(defun ref-or-exp (string)
  "Try to resolve string as a referent.  If string resolves as a
referent, the referent is returned.  If it cannot be resolved as a
referent, try to parse it as an expression \(and return the
expression\).  If string cannot be parsed as an expression,
parse-expression signals an error."
  (let ((ref? (resolve-referent string)))
    (if (not (eq ref? string)) ref?
      (read-expression-from-string string))))

;;; Mapping XML Elements and DRS Condition Types

;; The next three definitions, (*element-name/drs-condition-types*,
;; install-element-name-drs-condition-type-mapping, and
;; element-name-drs-condition-type, drs-condition-from-element)
;; provide an abstraction over the mapping between XML element names
;; and DRS Condition Types. This mechanism is designed to make it
;; easier to extend the DRS system if new condition types are added in
;; the future. The function drs-condition-from-element retrieves a drs
;; condition type based on an element's tag name, and then invokes a
;; method on the element that dispatches on the drs condition type,
;; building a drs condition of the correct type from the element.
;; Then, the macro define-drs-condition-extractor, is provided to
;; combine the mapping between element names and DRS condition types
;; to the procedural code that actually extracts the conditions from
;; the elements.
;;
;; The end result is that to extend the DRS in future, say with a new
;; type of condition called foo, which is represented with XML
;; elements named "Phieu", one simply writes an appropriate class
;; definition,
;;   (define-drs-condition foo ...),
;; and then a method for extracting conditions from XML elements,
;;   (define-drs-condition-extractor "Phieu" foo (element) ...).

(defvar *element-name/drs-condition-types*
  (make-hash-table :test 'equal :size 17)
  "A hash table mapping DRSXML element names to symbols that name the
classes corresponding to the correspdonding DRS conditions.")

(defun install-element-name-drs-condition-type-mapping 
       (element-name drs-condition-type)
  "* Syntax:
install-element-name-drs-condition-type-mapping
  element-name drs-condition-type => drs-condition-name
* Arguments and Values:
- element-name --- a string
- drs-condition-type --- a class, or symbol that names a class
* Description:
Causes element-name to be a key in
{defvar ace-drs:*element-name/drs-condition-types*} mapping
to drs-condition-name."
  (setf (gethash element-name *element-name/drs-condition-types*)
        drs-condition-type))

(defun element-name-drs-condition-type (element-name)
  "* Syntax:
element-name-drs-condition-type element-name => drs-condition-type, present-p
* Arguments and Values:
- element-name --- a string
- drs-condition-type --- a class, or symbol naming a class, or nil
- present-p --- a boolean
* Description:
If there is a class of drs-conditions that correspond to DRSXML
elements named by element-name, drs-condition-type is such a class, or
symbol naming such a class, and present-p is true. Otherwise
drs-condition-type and present-p are nil."
  (gethash element-name *element-name/drs-condition-types*))

(defun drs-condition-from-element (element)
  "* Syntax:
drs-condition-from-node node => drs-condition
* Arguments and Values:
- element --- a DOM element
- drs-condition --- a {defclass ace-drs:drs-condition}
* Description:
Extracts a drs-condition from element. The type of {defclass ace-drs:drs-condition} to
build is determined based on the element's tag name \(based on a mapping established by
{defmacro ace-drs:define-drs-condition-extractor} \)
and then {defgeneric ace-drs:extract-drs-condition-from-element} is applied to the
type the element. The resulting drs-condition is returned.
* Exceptional Situations:
If the element's tag name does not correspond to a class, an error is
signalled."
  (multiple-value-bind (drs-condition-type present-p)
      (element-name-drs-condition-type (dom:tag-name element))
    (if present-p (extract-drs-condition-from-element
                   drs-condition-type element)
      (error "Unknown element type ~S."
             (dom:tag-name element)))))

(defgeneric extract-drs-condition-from-element (kind element)
  (:documentation "* Syntax:
extract-drs-condition-from-node kind element => drs-condition
* Arguments and Values:
- kind --- a class, or a symbol that names a class
- node --- a DOM element
* Description:
Extract-drs-condition-from-element builds a drs-condition, specifically
of type kind from element. How this particular process is carried out
depends on the kind of object being constructed.
  Extract-drs-condition-from-element should usually not be called by
user code directly, but rather by
{defun ace-drs:drs-condition-from-element} .  Similarly, it is not expected
that extract-condition-from-element methods will be defined directly,
but rather using {defmacro ace-drs:define-drs-condition-extractor} .
* See Also:
- {defun ace-drs:drs-condition-from-element}
- {defmacro ace-drs:define-drs-condition-extractor} "))

(defun extract-drs-conditions-from-node-list (node-list)
  "* Syntax:
extract-drs-conditions-from-node-list node-list => conditions
* Arguments and Values:
- node-list --- a DOM node list
- conditions --- a list of {defclass ace-drs:drs-condition}
* Description:
Returns a list of drs-conditions extracted from the DOM element nodes
in node list. Conditions are extracted from element nodes using
{defun ace-drs:drs-condition-from-element} ."
  (map-element-nodes 'drs-condition-from-element node-list))

(defmacro define-drs-condition-extractor
          (element-name class-name (element) &body body)
  "* Syntax:
define-drs-condition-extractor
  element-name class-name (element) [[declaration* | documentation ]] form*
* Arguments and Values:
- element-name --- a string
- class-name --- a symbol or class, not evaluated
- element --- a variable
* Description:
define-drs-condition-extractor associates element-name with class-name
\(via {defun ace-drs:install-element-name-drs-condition-type-mapping} \), and
defines a method on {defgeneric ace-drs:extract-drs-condition-from-element}
whose first parameter is specialized with \(eql class-name\), and whose
second parameter is non-specialized, and named node. Declarations,
documentation, and forms appear as the body of the method. The body
should construct and return a drs-condition of type class-name from the
element.
* See Also:
- {defgeneric ace-drs:extract-drs-condition-from-element}
- {defun ace-drs:install-element-name-drs-condition-type-mapping}"
  (let ((kind (make-symbol "KIND")))
    `(progn
       (install-element-name-drs-condition-type-mapping 
	,element-name ',class-name)
       (defmethod extract-drs-condition-from-element
                  ((,kind (eql ',class-name)) ,element)
         ,@body))))

#+:lispworks
(progn
  (editor:setup-indent "define-drs-condition-extractor" 3)
  (dspec:define-form-parser define-drs-condition-extractor (ename cname)
    (declare (ignore define-drs-condition-extractor ename))
    `(defmethod extract-drs-condition-from-element ((eql ',cname) t))))

(defun parse-drsxml (blob)
  "* Syntax:
parse-drsxml blob => drs
* Arguments and Values:
- blob --- a string (or other suitable input for cxml:parse)
- drs --- a {defclass ace-drs:drs} object
* Description:
{defun ace-drs:parse-drsxml} parses the XML representation of a DRS in
the format produced by the APE Webservice.  The XML representation is
parsed by Closure XML [1], particularly using the function cxml:parse
[2].  cxml:parse can accept strings, pathnames, octet vectors or
streams.
* See Also:
- [1] {http://common-lisp.net/project/cxml/}
- [2] {http://common-lisp.net/project/cxml/sax.html}"
  (let* ((document (cxml:parse blob (cxml-dom:make-dom-builder)))
         (element (dom:document-element document)))
    (assert (string= "DRS" (dom:tag-name element)))
    (extract-drs-condition-from-element 'drs element)))

(define-drs-condition-extractor "DRS" drs (node)
  ;;;; <DRS domain=\"A B C\">
  ;;;;   <object ... />
  ;;;;   <object ... />
  ;;;;   <predicate ... />
  ;;;; </DRS>  
  (let* ((domain (dom:get-attribute node "domain"))
         (names (if (string= "" domain) '()
                  (split-sequence " " (dom:get-attribute node "domain"))))
         (domain (loop :for name :in names
                       :collecting (make-instance 'referent :name name)))
         (*referents* (cons domain *referents*)))
    (make-instance 'drs
                   :domain domain
                   :conditions (extract-drs-conditions-from-node-list
                                (dom:child-nodes node)))))

(defun parse-object-count (string)
  "* Syntax:
parse-object-count string => result
* Arguments and Values:
- string --- a string
- result --- a number, or the symbol :na
* Description:
parse-object-count returns the count described by string. The ACE DRS
6.0 report, section 2.4.1 states that count is a positive number or
\"na\". If string is \"na\", then the keyword :na is returned.
Otherwise the number is read from the string and returned.
* Exceptional Situations:
parse-object-count, in the event that string is not \"na\" ASSERTs
that the value read from the string is a real and is positive."
  (if (string= string "na") :na
    (parse-real string)))

(defun parse-object-unit (string)
  "* Syntax:
parse-object-unit string => result
- string --- a string
- result --- a string or the symbol :na
* Description:
parse-object-unit returns the measurement unit contained in string, or
:na if string is \"na\"."
  (if (string= string "na") :na
    string))

(define-drs-condition-extractor "object" object (node)
  ;;;; <object ref="A"
  ;;;;         noun="John"
  ;;;;         struct="named"
  ;;;;         unit="na"
  ;;;;         numrel="eq"
  ;;;;         num="1"
  ;;;;         sentid="1"/>
  (make-node-instance 'object node
    :ref '("ref" :key resolve-referent)
    :noun "noun"
    :quant '("struct" :key keywordify)
    :unit '("unit" :key parse-object-unit)
    :op '("numrel" :key keywordify)
    :count '("num" :key parse-object-count)
    ;; FIXME: don't actually know what the attribute name is for type
    ;; (throughout) and for dim-type
    :type '("type" :optional t)
    :dim-type '("dimType" :optional t) 
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(define-drs-condition-extractor "property" property (node)
  ;; Property elements are constructed in three different ways, and
  ;; the the three cases use different attribute names...
  (cond 
   ;; ditransitive
   ;;;; <property ref="D"
   ;;;;           adj="fond-of"
   ;;;;           obj1="B"
   ;;;;           degree="comp_than"
   ;;;;           comptarget="obj"
   ;;;;           obj2="C"
   ;;;;           sentid="1"/>
   ((dom:has-attribute node "obj1")
    (make-node-instance 'property node
      :ref1 '("ref" :key ref-or-exp)
      :adjective "adj"
      :ref2 '("obj1" :key ref-or-exp)
      :degree '("degree" :key keywordify)
      :comp-target '("comptarget" :key keywordify)
      :ref3 '("obj2" :key ref-or-exp)
      :type '("type" :optional t)
      :sentid '("sentid" :key parse-integer)
      :tokid '("tokid" :key parse-integer)))
   ;; transitive
   ;;;; <property ref="C"
   ;;;;           adj="happy"
   ;;;;           degree="comp_than"
   ;;;;           obj="B"
   ;;;;           sentid="1"/>
   ((dom:has-attribute node "obj")
    (make-node-instance 'property node
      :ref1 '("ref" :key ref-or-exp)
      :adjective "adj"
      :degree '("degree" :key keywordify)
      :ref2 '("obj" :key ref-or-exp)
      :type '("type" :optional t)
      :sentid '("sentid" :key parse-integer)
      :tokid '("tokid" :key parse-integer)))
   ;; intransitive
   ;;;; <property ref="B"
   ;;;;           adj="happy"
   ;;;;           degree="pos"
   ;;;;           sentid="1"/>
   (t (make-node-instance 'property node
        :ref1 '("ref" :key ref-or-exp)
        :adjective "adj"
        :degree '("degree" :key keywordify)
        :type '("type" :optional t)
        :sentid '("sentid" :key parse-integer)
        :tokid '("tokid" :key parse-integer)))))

(define-drs-condition-extractor "relation" relation (node)
  ;; <relation obj1="C" rel="of" obj2="B" sentid="1"/>
  (make-node-instance 'relation node
    :ref1 '("obj1" :key resolve-referent)
    :ref2 '("obj2" :key ref-or-exp)
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(define-drs-condition-extractor "predicate" predicate (node)
  ;; Predicates, like properties also have several possible forms.
  (cond
   ;;;; <predicate ref="D"
   ;;;;            verb="give"
   ;;;;            subj="A"
   ;;;;            obj="C"
   ;;;;            indobj="B"
   ;;;;            sentid="1" />
   ((dom:has-attribute node "indobj")
    (make-node-instance 'predicate node
      :ref '("ref" :key resolve-referent)
      :verb "verb"
      :subj-ref '("subj" :key ref-or-exp)
      :obj-ref '("obj" :key ref-or-exp)
      :ind-obj-ref '("indobj" :key ref-or-exp)
      :type '("type" :optional t)
      :sentid '("sentid" :key parse-integer)
      :tokid '("tokid" :key parse-integer)))
   ;;;; <predicate ref="C"
   ;;;;            verb="give"
   ;;;;            subj="A"
   ;;;;            obj="B"
   ;;;;            sentid="1" />
   ((dom:has-attribute node "obj")
    (make-node-instance 'predicate node
      :ref '("ref" :key resolve-referent)
      :verb "verb"
      :subj-ref '("subj" :key ref-or-exp)
      :obj-ref '("obj" :key ref-or-exp)
      :type '("type" :optional t)
      :sentid '("sentid" :key parse-integer)
      :tokid '("tokid" :key parse-integer)))
   ;;;; <predicate ref="B"
   ;;;;            verb="give"
   ;;;;            subj="A"
   ;;;;            sentid="1" />
   (t (make-node-instance 'predicate node
        :ref '("ref" :key resolve-referent)
        :verb "verb"
        :subj-ref '("subj" :key ref-or-exp)
        :type '("type" :optional t)
        :sentid '("sentid" :key parse-integer)
        :tokid '("tokid" :key parse-integer)))))

(define-drs-condition-extractor "modifier_adv" modifier-adv (node)
  ;;;; <modifier_adv ref="D"
  ;;;;               adverb="swiftly"
  ;;;;               degree="pos"
  ;;;;               sentid="1" />
  (make-node-instance 'modifier-adv node
    :ref '("ref" :key resolve-referent)
    :adverb "adverb"
    :degree '("degree" :key keywordify)
    :type '("type" :optional t)
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(defun parse-modifier-pp-type (string)
  "* Syntax:
parse-modifier-pp-type string => result
* Arguments and Values:
- string --- a string, should be \"na\"
- result --- the keyword :na
* Description:
The ACE DRS 6.0 document, section 2.4.6 states that a modifier_pp's
type is always \"na\".
* Exceptional Situations:
parse-modifier-pp-type ASSERTs that string is string= to \"na\"."
  (prog1 :na
    (assert (string= string "na") (string))))

(define-drs-condition-extractor "modifier_pp" modifier-pp (node)
  ;;;; <modifier_pp ref="C"
  ;;;;              prep="into"
  ;;;;              obj="D"
  ;;;;              sentid="1"/>
  (make-node-instance 'modifier-pp node
    :ref1 '("ref" :key resolve-referent)
    :preposition "prep"
    :ref2 '("obj" :key ref-or-exp)
    :type '("type" :key parse-modifier-pp-type :optional t)
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(define-drs-condition-extractor "has_part" has-part (node)
  ;;;; <has_part group="B"
  ;;;;           member="C"
  ;;;;           sentid="1" />
  (make-node-instance 'has-part node
    :group-ref '("group" :key resolve-referent)
    :member-ref '("member" :key ref-or-exp)
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(define-drs-condition-extractor "query" query (node)
  ;;;; <query obj="B"
  ;;;;        question="who"
  ;;;;        sentid="1" />
  (make-node-instance 'query node
    :ref '("obj" :key resolve-referent)
    :question-word '("question" :key keywordify)
    :sentid '("sentid" :key parse-integer)
    :tokid '("tokid" :key parse-integer)))

(defmacro define-simple-complex-condition-extractor
          (element-name class-name &optional (initarg :drs))
  (let ((node (gensym (string '#:node-)))
        (drs (gensym (string '#:drs-))))
    `(define-drs-condition-extractor ,element-name ,class-name (,node)
       (destructuring-bind (,drs)
           (extract-drs-conditions-from-node-list
            (dom:child-nodes ,node))
         (make-instance ',class-name ,initarg ,drs)))))

#+:lispworks
(progn
  (editor:setup-indent "define-simple-complex-condition-extractor" 2)
  (dspec:define-form-parser define-simple-complex-condition-extractor (ename cname)
    (declare (ignore define-simple-complex-condition-extractor ename))
    `(defmethod extract-drs-condition-from-element ((eql ',cname) t))))

(define-simple-complex-condition-extractor "Negation" negation)
(define-simple-complex-condition-extractor "NAF" negation-as-failure)

(define-drs-condition-extractor "Implication" implication (node)
  "* Syntax:
extract-implication-from-node node => implication
* Arguments and Values:
- node --- a dom:node
- implication --- a {defclass ace-drs:implication}
* Description:
The DOM node should describe an element of the form:
;;;   <Implication>
;;;     <DRS domain=\"C\"> ... </DRS>
;;;     <DRS domain=\"D\"> ... </DRS>
;;;   </Implication>
The first DRS element \(the antecedent of the implication\) is parsed,
and then the second DRS element \(the consequent of the implication\)
is parsed with the domain of the antecdent in scope. That is, the
conditions of the consequent may include references to elements in the
domain of the antecdent. \(This behavior is described by Section 2.5.3
of the ACE 6.0 DRS Report.\)"
  (let* ((drses (map-element-nodes 'identity (dom:child-nodes node)))
         (antecedent (extract-drs-condition-from-element
                      'drs (first drses)))
         (*referents* (cons (drs-domain antecedent) *referents*))
         (consequent (extract-drs-condition-from-element
                      'drs (second drses))))
    (make-instance 'implication
                   :antecedent antecedent
                   :consequent consequent)))

(define-drs-condition-extractor "Disjunction" disjunction (node)
  ;;;;   <Disjunction>
  ;;;;     <DRS domain="D"> ... </DRS>
  ;;;;     <DRS domain="E"> ... </DRS>
  ;;;;   </Disjunction>
  (destructuring-bind (drs1 drs2)
      (extract-drs-conditions-from-node-list
       (dom:child-nodes node))
    (make-instance 'disjunction :drs1 drs1 :drs2 drs2)))

(define-simple-complex-condition-extractor "Possibility" possibility)
(define-simple-complex-condition-extractor "Necessity" necessity)
(define-simple-complex-condition-extractor "Recommendation" recommendation)
(define-simple-complex-condition-extractor "Admissibility" admissibility)

(define-drs-condition-extractor "Proposition" proposition (node)
  ;;;;   <Proposition ref="E">
  ;;;;     <DRS domain="F G"> ... </DRS>
  ;;;;   </Proposition>
  (destructuring-bind (drs)
      (extract-drs-conditions-from-node-list
       (dom:child-nodes node))
    (let* ((ref (dom:get-attribute node "ref"))
           (ref (resolve-referent ref)))
      (make-instance 'proposition :drs drs :ref ref))))

(define-simple-complex-condition-extractor "Question" question)
(define-simple-complex-condition-extractor "Command"  command)
