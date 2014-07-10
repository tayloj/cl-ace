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

;;;; Referents

(defclass referent ()
  ((name
    :type string
    :reader referent-name
    :initarg :name
    :documentation
    "The name of the referent, preserved from the DRSXML.")
   (referers
    :initform '()
    :reader referent-referers
    :initarg :referers
    :documentation
    "Predicates that refer to the referent.")
   (defining-conditions
    :initform '()
    :reader referent-defining-conditions
    :initarg :defining-conditions
    :documentation
    "DRS conditions which define the referent."))
  (:documentation ""))

(defmethod print-object ((referent referent) out-stream)
  (print-unreadable-object (referent out-stream :type t :identity t)
    (write (referent-name referent) :stream out-stream)))

(defun refer-to-referent (referer referent)
  "Add referer to referent's referers."
  (pushnew referer (slot-value referent 'referers)))

(defun define-referent (definer referent)
  "Add definer to referent's defining-conditions."
  (pushnew definer (slot-value referent 'defining-conditions)))

;;;; Expressions

(defclass expression () ()
  (:documentation "Expression is a parent class of all ACE
expressions.  Expressions correspond to Prolog terms.  The classes
representing expressions are:
- {defclass ace-drs:named-expression}
- {defclass ace-drs:int-expression}
- {defclass ace-drs:real-expression}
- {defclass ace-drs:string-expression}
- {defclass ace-drs:compound-expression}
- {defclass ace-drs:list-expression}
- {defclass ace-drs:set-expression}"))

;;; Proper Names

(defclass named-expression (expression)
  ((name
    :type string
    :reader named-expression-name
    :initarg :name
    :documentation "The string that is the proper name."))
  (:documentation "Named expressions denote objects with proper names."))

(defmethod print-object ((n named-expression) out)
  (print-unreadable-object (n out)
    (format out "named(~A)" (named-expression-name n))))

;;; Atomic Expressions

;; Integers

(defclass int-expression (expression)
  ((integer
    :type integer
    :reader int-expression-integer
    :initarg :integer
    :documentation "The number of the integer expression."))
  (:documentation "Int-expressions denote integer values."))

(defmethod print-object ((i int-expression) out)
  (print-unreadable-object (i out)
    (format out "int(~A)" (int-expression-integer i))))

;; Reals

(defclass real-expression (expression)
  ((real
    :type real
    :reader real-expression-real
    :initarg :real
    :documentation "The real of the real expression."))
  (:documentation "Real-expressions denote real (number) values."))

(defmethod print-object ((r real-expression) out)
  (print-unreadable-object (r out)
    (format out "real(~A)" (real-expression-real r))))

;; Strings

(defclass string-expression (expression)
  ((string
    :type string
    :reader string-expression-string
    :initarg :string
    :documentation "The string of the string expression."))
  (:documentation "String-expressions denote strings."))

(defmethod print-object ((s string-expression) out)
  (print-unreadable-object (s out)
    (format out "string(~A)" (string-expression-string s))))

;;; Compound Expressions

(defclass compound-expression (expression)
  ((operator
    :type (member :plust :minus :times :divide :concatenate)
    :reader compound-expression-operator
    :initarg :operator
    :documentation "The operator of the compound expression.")
   (arguments
    :type list
    :reader compound-expression-arguments
    :initarg :arguments
    :documentation
    "The arguments of the compound expression.  Example suggest that
     compound expressions always have exactly two arguments."))
  (:documentation
   "Compound expressions denote the application of an operator to a
    sequence of arguments."))

(defmethod print-object ((c compound-expression) out)
  (print-unreadable-object (c out)
    (format out "expr(~A~{,~A~})"
            (compound-expression-operator c)
            (compound-expression-arguments c))))

;;; Lists and Sets

(defclass list-expression (expression)
  ((elements
    :type list
    :reader list-expression-elements
    :initarg :elements
    :documentation "The elements of the list expression."))
  (:documentation "List-expressions denote lists."))

(defmethod print-object ((l list-expression) out)
  (print-unreadable-object (l out)
    (format out "list([~{~A~^,~}])" (list-expression-elements l))))

(defclass set-expression (expression)
  ((elements
    :type list
    :reader set-expression-elements
    :initarg :elements
    :documentation "The elements of the set expression."))
  (:documentation "Set-expressions denote sets."))

(defmethod print-object ((s set-expression) out)
  (print-unreadable-object (s out)
    (format out "set({~{~A~^,~}})" (set-expression-elements s))))

;;;; Conditions

(defclass drs-condition () ()
  (:documentation "drs-condition is the class of object which can
appear in the conditions of a {defclass ace-drs:drs} . It is a parent
class to all of the various types of drs conditions, and should never
be instantiated directly. The types of drs conditions are:
- {defclass ace-drs:drs}
- Simple Conditions
-- {defclass ace-drs:object}
-- {defclass ace-drs:property}
-- {defclass ace-drs:relation}
-- {defclass ace-drs:predicate}
-- {defclass ace-drs:modifier-adv}
-- {defclass ace-drs:modifier-pp}
-- {defclass ace-drs:has-part}
-- {defclass ace-drs:query}
- Complex Conditions
-- {defclass ace-drs:negation}
-- {defclass ace-drs:negation-as-failure}
-- {defclass ace-drs:implication}
-- {defclass ace-drs:disjunction}
-- {defclass ace-drs:possibility}
-- {defclass ace-drs:necessity}
-- {defclass ace-drs:recommendation}
-- {defclass ace-drs:admissibility}
-- {defclass ace-drs:proposition}
-- {defclass ace-drs:question}
-- {defclass ace-drs:command}"))

(defmethod initialize-instance :after ((dc drs-condition) &rest initargs)
  "The :AFTER method for INITIALIZE-INSTANCE for objects of type
drs-condition calls initialize-drs-condition on the object."
  (declare (ignore initargs))
  (initialize-drs-condition dc))

(defgeneric initialize-drs-condition (condition)
  (:documentation "* Syntax
initialize-drs-condition object => result*
* Arguments and Values:
- object --- an object
* Description
Initialize-drs-condition is called by an :AFTER method on
INITIALIZE-INSTANCE for objects which are instances of 
{defclass ace-drs:drs-condition} . Initialize-drs-condition
is responsible for connecting the condition's references to
their referents. An initialize-drs-condition method is
automatically defined for each class defined with
{defmacro ace-drs:define-drs-condition} .
* Notes:
There should be no reason for user code to define methods on
initialize-drs-condition. The behavior provided by
{defmacro ace-drs:define-drs-condition} should be sufficient.")
  (:method ((dc drs-condition))
   (declare (ignore dc))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-drs-condition-initializer (class-name references definitions)
  (let ((object (gensym (string '#:object-)))
        (slot (gensym (string '#:slot-))))
    `(defmethod initialize-drs-condition ((,object ,class-name))
       (dolist (,slot ',references)
         (when (and (slot-boundp ,object ,slot)
                    (typep (slot-value ,object ,slot) 'referent))
           (refer-to-referent ,object (slot-value ,object ,slot))))
       (dolist (,slot ',definitions)
         (when (and (slot-boundp ,object ,slot)
                    (typep (slot-value ,object ,slot) 'referent))
           (define-referent ,object (slot-value ,object ,slot)))))))
) ; eval-when

(defmacro define-drs-condition (name superclasses slots &rest options)
  "* Syntax:
define-drs-condition class-name superclasses slots [[class-option]]
* Arguments and Values:
The arguments to define-drs-condition are like DEFCLASS, with the
addition of one new kind of class option, :references.
* Description:
define-drs-condition defines a class named name, that inherits from
superclasses as well as {defclass ace-drs:drs-condition} , has the
specified slots and class options. An addition class option may be
\(:references {slot-name}*\), which causes the named slots to be
resolved when parsing from DRSXML---that is, for each of the named
slots that is bound during initialize, the drs-condition stored into
the referent's list of referers."
  (let* ((references (find :references options :key 'first))
         (reference-slots (rest references))
         (defines (find :defines options :key 'first))
         (definition-slots (rest defines))
         (options (delete :references options :key 'first))
         (options (delete :defines    options :key 'first)))
    `(progn
       (defclass ,name (drs-condition ,@superclasses)
         ,slots ,@options)
       ,@(unless (null references)
           (list
            (make-drs-condition-initializer
             name
             reference-slots
             definition-slots))))))

#+:lispworks
(progn 
  (editor:setup-indent "define-drs-condition" 2)
  (dspec:define-form-parser (define-drs-condition (:alias defclass)))
  (dspec:define-dspec-alias define-drs-condition (name)
    `(defclass ,name)))

;;; Condition Mixins

(defclass sentid-mixin ()
  ((sentid
    :type (integer 1)
    :reader sentid
    :initarg :sentid
    :documentation
    "An integer greater than 0 indicating the sentence of which a
     component corresponds to the object.")
   (tokid
    :type (integer 1)
    :reader tokid
    :initarg :tokid
    :documentation
    "An integer greater than 0 indicating which token within the
     sentence the object corresponds to."))
  (:documentation
   "The class of objects that can be associated with a sentence."))

(defclass type-mixin ()
  ((type
    :type string
    :reader type
    :initarg :type
    :documentation
    "The type of the thing when the DRS is typed."))
  (:documentation
   "The class of objects that can be typed."))

;;; Simple Conditions

;; Objects

(deftype object-quant ()
  "Quantities are one of :dom, :mass, :countable, and
indicate the quantisation of an object."
  '(member :dom :mass :countable))

(deftype object-op ()
  "One of :eq, :geq, :greater, :exactly, :na."
  '(member :eq :geq :greater :exactly :na))

(define-drs-condition object (type-mixin sentid-mixin)
  ((ref
    :type referent
    :reader object-ref
    :initarg :ref)
   (noun
    :type string
    :reader object-noun
    :initarg :noun)
   (quant
    :type object-quant
    :reader object-quant
    :initarg :quant)
   (unit
    :type string
    :reader object-unit
    :initarg :unit)
   (op
    :type object-op
    :reader object-op
    :initarg :op)
   (count
    :type (satisfies plusp)
    :reader object-count
    :initarg :count)
   (type
    :reader object-type)
   (dim-type
    :reader object-dim-type
    :initarg :dim-type)
   (sentid
    :reader object-sentid))
  (:defines ref)
  (:references ref))

;; Properties

(deftype property-degree ()
  "Property degree values are one of the following: :pos, :pos-as,
:comp, :comp-than, and :sup."
  '(member :pos :pos-as :comp :comp-than :sup))

(deftype property-comp-target ()
  "Property comp-target values are either :subj, or :obj, indicating
whether the target of the comparison is the subject or the object of
the sentence."
  '(member :subj :obj))

(define-drs-condition property (type-mixin sentid-mixin)
  ((ref1
    :type (or referent expression)
    :reader property-ref1
    :initarg :ref1)
   (adjective
    :type string
    :reader property-adjective
    :initarg :adjective)
   (ref2
    :type (or referent expression)
    :reader property-ref2
    :initarg :ref2)
   (degree
    :type property-degree
    :reader property-degree
    :initarg :degree)
   (comp-target
    :type property-comp-target
    :reader property-comp-target
    :initarg :comp-target)
   (ref3
    :type (or referent expression)
    :reader property-ref3
    :initarg :ref3)
   (type
    :reader property-type)
   (sentid
    :reader property-sentid))
  (:references ref1 ref2 ref3)
  (:defines ref1))

;; Relations

(define-drs-condition relation (sentid-mixin)
  ((ref1
    :type referent
    :reader relation-ref1
    :initarg :ref1)
   (ref2
    :type (or referent expresion)
    :reader relation-ref2
    :initarg :ref2)
   (sentid
    :reader relation-sentid))
  (:references ref1 ref2))

;; Predicates

(define-drs-condition predicate (type-mixin sentid-mixin)
  ((ref
    :type referent
    :reader predicate-ref
    :initarg :ref)
   (verb
    :type string
    :reader predicate-verb
    :initarg :verb)
   (subj-ref
    :type (or referent expression)
    :reader predicate-subj-ref
    :initarg :subj-ref)
   (obj-ref
    :type (or referent expression)
    :reader predicate-obj-ref
    :initarg :obj-ref)
   (ind-obj-ref
    :type (or referent expression)
    :reader predicate-ind-obj-ref
    :initarg :ind-obj-ref)
   (type
    :reader predicate-type)
   (sentid
    :reader predicate-sentid))
  (:references ref subj-ref obj-ref ind-obj-ref)
  (:defines ref))

;; Modifier-adv

(deftype modifier-adv-degree ()
  "Possible adverb modifier degrees: :pos, :comp, :sup."
  '(member :pos :comp :sup))

(define-drs-condition modifier-adv (type-mixin sentid-mixin)
  ((ref
    :type referent
    :reader modifier-adv-ref
    :initarg :ref)
   (adverb
    :type string
    :reader modifier-adv-adverb
    :initarg :adverb)
   (degree
    :type modifier-adv-degree
    :reader modifier-adv-degree
    :initarg :degree)
   (type
    :reader modifier-adv-type)
   (sentid
    :reader modifier-adv-sentid))
  (:references ref))

;; Modifier-pp

(define-drs-condition modifier-pp (type-mixin sentid-mixin)
  ((ref1
    :type referent
    :reader modifier-pp-ref1
    :initarg :ref1)
   (preposition
    :type string
    :reader modifier-pp-preposition
    :initarg :preposition)
   (ref2
    :type (or referent expression)
    :reader modifier-pp-ref2
    :initarg :ref2)
   (type
    :reader modifier-pp-type)
   (sentid
    :reader modifier-pp-sentid))
  (:references ref1 ref2))

;; Has-part

(define-drs-condition has-part (sentid-mixin)
  ((group-ref
    :type referent
    :reader has-part-group-ref
    :initarg :group-ref)
   (member-ref
    :type (or referent expression)
    :reader has-part-member-ref
    :initarg :member-ref)
   (sentid
    :reader has-part-sentid))
  (:references group-ref member-ref))

;; Queries

(deftype query-question-word ()
  "Possible values for query question words: :who, :what, :which,
:how, :where, :when."
  '(member :who :what :which :how :where :when))

(define-drs-condition query (sentid-mixin)
  ((ref
    :type referent
    :reader query-ref
    :initarg :ref)
   (question-word
    :type query-question-word
    :reader query-question-word
    :initarg :question-word)
   (sentid
    :reader query-sentid))
  (:references ref))

;;; Complex Conditions

;; Classical Negation

(define-drs-condition negation ()
  ((drs
    :reader negation-drs
    :initarg :drs)))

(defmethod print-object ((n negation) out)
  (print-unreadable-object (n out :identity t)
    (write-string "NOT " out)
    (write (negation-drs n) :stream out)))

;; Negation as Failure

(define-drs-condition negation-as-failure ()
  ((drs
    :reader negation-as-failure-drs
    :initarg :drs)))

(defmethod print-object ((n negation-as-failure) out)
  (print-unreadable-object (n out :identity t)
    (write-string "NAF " out)
    (write (negation-as-failure-drs n) :stream out)))

;; Implication and Disjunction

(define-drs-condition implication ()
  ((antecedent
    :reader implication-antecedent
    :initarg :antecedent)
   (consequent
    :reader implication-consequent
    :initarg :consequent)))

(defmethod print-object ((i implication) out)
  (print-unreadable-object (i out :identity t)
    (write (implication-antecedent i) :stream out)
    (write-string " => " out)
    (write (implication-consequent i) :stream out)))

(define-drs-condition disjunction ()
  ((drs1
    :reader disjunction-drs1
    :initarg :drs1)
   (drs2
    :reader disjunction-drs2
    :initarg :drs2)))

(defmethod print-object ((d disjunction) out)
  (print-unreadable-object (d out :identity t)
    (write (disjunction-drs1 d) :stream out)
    (write-string " V " out)
    (write (disjunction-drs2 d) :stream out)))

;; Possibility and Necessity

(define-drs-condition possibility ()
  ((drs
    :reader possibility-drs
    :initarg :drs)))

(defmethod print-object ((p possibility) out)
  (print-unreadable-object (p out :identity t)
    (write-string "<> " out)
    (write (possibility-drs p) :stream out)))

(define-drs-condition necessity ()
  ((drs
    :reader necessity-drs
    :initarg :drs)))

(defmethod print-object ((n necessity) out)
  (print-unreadable-object (n out :identity t)
    (write-string "[] " out)
    (write (necessity-drs n) :stream out)))

;; Recommendation and Admissibility

(define-drs-condition recommendation ()
  ((drs
    :reader recommendation-drs
    :initarg :drs)))

(defmethod print-object ((r recommendation) out)
  (print-unreadable-object (r out :identity t)
    (write-string "SHOULD " out)
    (write (recommendation-drs r) :stream out)))

(define-drs-condition admissibility ()
  ((drs
    :reader admissibility-drs
    :initarg :drs)))

(defmethod print-object ((a admissibility) out)
  (print-unreadable-object (a out :identity t)
    (write-string "MAY " out)
    (write (admissibility-drs a) :stream out)))

;; Sentence Subordination

(define-drs-condition proposition ()
  ((ref
    :type referent
    :reader proposition-ref
    :initarg :ref)
   (drs
    :type drs
    :reader proposition-drs
    :initarg :drs))
  (:references ref)
  (:defines ref))

(defmethod print-object ((p proposition) out)
  (print-unreadable-object (p out :identity t)
    (write (proposition-ref p) :stream out)
    (write-string ":" out)
    (write (proposition-ref p) :stream out)))

;; Questions and Commands

(define-drs-condition question ()
  ((drs
    :reader question-drs
    :initarg :drs)))

(defmethod print-object ((q question) out)
  (print-unreadable-object (q out :identity t)
    (write-string "QUESTION " out)
    (write (question-drs q) :stream out)))

(define-drs-condition command ()
  ((drs
    :reader command-drs
    :initarg :drs)))

(defmethod print-object ((c command) out)
  (print-unreadable-object (c out :identity t)
    (write-string "COMMAND " out)
    (write (command-drs c) :stream out)))

;; Discourse Representation Structures (DRS)

(define-drs-condition drs ()
  ((domain
    :type list ; of referents
    :reader drs-domain
    :initarg :domain)
   (conditions
    :type list
    :reader drs-conditions
    :initarg :conditions)))

#-(and)
(defmethod print-object ((d drs) out)
  (print-unreadable-object (d out :identity t)
    (write (drs-domain d) :stream out)
    (write-char #\Space out)
    (dolist (c (drs-conditions d))
      (terpri out)
      (write c :stream out))))
                         
