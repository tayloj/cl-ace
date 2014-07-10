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

(in-package "COMMON-LISP")

(defpackage #:cl-ace-util
  (:use "COMMON-LISP")
  (:documentation
   "Some utilities used by both APE-WEBSERVICE and ACE-DRS.")
  (:export
   #:define-restart-function
   #:remf*
   #:do-element-nodes
   #:map-element-nodes
   #:map-node-list
   #:text-content
   #:pcase
   #:multiple-value-prog2
   #:make-node-attribute-plist
   #:make-node-instance
   #:keywordify
   #:read-safely-from-string
   #:parse-type
   #:parse-float
   #:parse-real
   #:strcat
   #:split-sequence
   #:prompt-for-line
   #:with-namespaces
   #:with-soap-envelope))

(defpackage #:soap
  (:use "COMMON-LISP" #:cl-ace-util)
  (:export
   #:with-soap-envelope)
  (:documentation
   "At the moment, the SOAP package contains just enough SOAP related
things to get CL-ACE to work. Maybe in the future it can be a more
full-fledged SOAP implementation."))

(defpackage #:acerules-webservice
  (:use "COMMON-LISP" #:cl-ace-util #:soap)
  (:shadow #:step)
  (:documentation "")
  (:export
   #:+acerules-webservice+
   #:+acerules-prefix+
   #:invoke-acerules-webservice

   #:acerules-reply
   #:acerules-reply-rules        #:rules
   #:acerules-reply-simple-rules #:simple-rules
   #:acerules-reply-answersets   #:answersets
   #:acerules-reply-answertexts  #:answertexts
   #:acerules-reply-trace        #:trace
   #:acerules-reply-ace-trace    #:ace-trace

   #:step
   #:step-raw #:raw
   #:step-delete #:delete
   #:step-consistent #:consistent

   #:acerules))

(defpackage #:ape-webservice
  (:use "COMMON-LISP" #:cl-ace-util)
  (:documentation "The CL-ACE package implements the functionality
needed to call the APE webservice and process the structures that it
returns. The structures used in CL-ACE roughly match those in the
webservice documentation [1] and the XML Schema description of APE
output [2]. Names have been adjusted to typical Lisp case, and some
enumerated string values have become keywords. The two main interfaces
to the APE Webservice are the {defun ape-webservice:ape} and
{defun ape-webservice:invoke-ape-webservice} functions.  The ape
function is intended to be the primary interface to APE results, but
{defun ape-webservice:invoke-ape-webservice} is provided as a direct
interface to APE.
* See Also:
- [1] {http://attempto.ifi.uzh.ch/site/docs/ape_webservice.html}
- [2] {http://attempto.ifi.uzh.ch/site/docs/XMLSchema/apews_output.xsd}")
  (:export
   #:+ape-webservice-url+
   
   #:invoke-ape-webservice

   #:ape-condition
   
   #:ape-error
   #:ape-error-type

   #:fetch-acetext-error
   #:fetch-lexicon-error
   #:socket-init-error
   #:socket-connect-error
   #:socket-close-error
   #:logfile-open-error
   #:logfile-close-error
   #:ulex-open-error
   #:ulex-close-error
   
   #:ape-result
   #:ape-result-duration
   #:ape-result-input
   #:ape-result-tokens
   #:ape-result-sentences
   #:ape-result-drs
   #:ape-result-syntax
   #:ape-result-syntaxpp
   #:ape-result-syntaxd
   #:ape-result-syntaxdpp
   #:ape-result-drspp
   #:ape-result-drsxml
   #:ape-result-drshtml
   #:ape-result-paraphrase
   #:ape-result-paraphrase1
   #:ape-result-paraphrase2
   #:ape-result-owlrdf
   #:ape-result-owlfss
   #:ape-result-owlfsspp
   #:ape-result-owlxml
   #:ape-result-ruleml
   #:ape-result-fol
   #:ape-result-pnf
   #:ape-result-messages
   #:ape-result-timestamp
   
   #:duration
   #:duration-tokenizer
   #:duration-parser
   #:duration-refres

   #:message-importance
   #:message-type

   #:message
   #:message-importance
   #:message-type
   #:message-sentence
   #:message-token
   #:message-value
   #:message-repair

   #:ape-warning-message
   #:ape-error-message

   #:ape

   #:discard-owl
   #:use-file
   #:use-text
   #:use-ulexfile
   #:use-ulex
   ))

(defpackage #:ace-drs
  (:use "COMMON-LISP" #:cl-ace-util)
  (:documentation "The ACE-DRS package implements lisp structures that
correspond to those described in \"Discourse Representation Structure
for ACE 6.0\" [1], as well as a parser for the DRSXML output format.
As with CL-ACE, names have been adjusted for lisp conventions, and
some processing of values occurs. It should be noted that class slot
and reader names correspond to those described in the DRS report [1]
rather than the element and attribute names that appear in the DRSXML
output. The parsing interface for DRSXML is {defun ace-drs:parse-drsxml} .
{defun ace-drs:parse-drsxml} is also called by CL-ACE automatically
when APE-results contain DRSXML output.
  The renaming for Lisp conventions is as follows. The classes
corresponding to ACE entities use the names specified in the \"DRS for
ACE 6.0\" tech report, with the exceptions that: rather than mixed
case names, the source text is downcased, and #\- inserted at the
proper place, and #\_ have been converted to #\-. So, for instance,
\"has_part(GroupRef, MemberRef)\" becomes a class has-part with slots
group-ref and member-ref. Readers have been created which are
<class-name>-<slot-name>, e.g., has-part-group-ref, and initargs are
keywordized slot names, e.g., :group-ref.
* See Also: 
- [1] {http://attempto.ifi.uzh.ch/site/pubs/papers/drs_report_6.pdf}")
  (:shadow #:object #:type)
  ;; drsxml.lisp
  (:export
   #:drs-condition-from-element
   #:extract-drs-condition-from-element
   #:extract-drs-conditions-from-node-list
   #:define-drs-condition-extractor
   
   #:parse-drsxml)
  ;; expressions.lisp
  (:export
   #:read-expression
   #:read-expression-from-string)
  ;; drs.lisp
  (:export
   #:referent
   #:referent-name
   #:referent-referers
   #:referent-defining-conditions

   #:expression
   #:named-expression
   #:named-expression-name
   #:int-expression
   #:int-expression-integer
   #:real-expression
   #:real-expression-real
   #:string-expression
   #:string-expression-string
   #:compound-expression
   #:compound-expression-operator
   #:compound-expression-arguments
   #:list-expression
   #:list-expression-elements
   #:set-expression
   #:set-expression-elements

   #:drs-condition

   #:sentid-mixin
   #:sentid

   #:type-mixin
   #:type
   
   #:object-quant
   #:object-op 
   
   #:object
   #:object-ref
   #:object-noun
   #:object-quant
   #:object-unit
   #:object-op
   #:object-count
   #:object-type
   #:object-dim-type
   #:object-sentid

   #:property-degree
   #:property-comp-target

   #:property
   #:property-ref1
   #:property-adjective
   #:property-ref2
   #:property-degree
   #:property-comp-target
   #:property-ref3
   #:property-type
   #:property-sentid

   #:relation
   #:relation-ref1
   #:relation-ref2
   #:relation-sentid

   #:predicate
   #:predicate-ref
   #:predicate-verb
   #:predicate-subj-ref
   #:predicate-obj-ref
   #:predicate-ind-obj-ref
   #:predicate-type
   #:predicate-sentid

   #:modifier-adv-degree

   #:modifier-adv
   #:modifier-adv-ref
   #:modifier-adv-adverb
   #:modifier-adv-degree
   #:modifier-adv-type
   #:modifier-adv-sentid

   #:modifier-pp
   #:modifier-pp-ref1
   #:modifier-pp-preposition
   #:modifier-pp-ref2
   #:modifier-pp-type
   #:modifier-pp-sentid

   #:has-part
   #:has-part-group-ref
   #:has-part-member-ref
   #:has-part-sentid

   ;; quantity removed between ACE 6.0 and 6.5
   #:quantity-op
   
   #:quantity
   #:quantity-ref
   #:quantity-op
   #:quantity-count
   #:quantity-sentid

   #:query-question-word
   
   #:query
   #:query-ref
   #:query-question-word
   #:query-sentid

   #:negation
   #:negation-drs

   #:negation-as-failure
   #:negation-as-failure-drs

   #:implication
   #:implication-antecedent
   #:implication-consequent

   #:disjunction
   #:disjunction-drs1
   #:disjunction-drs2

   #:possibility
   #:possibility-drs

   #:necessity
   #:necessity-drs

   #:recommendation
   #:recommendation-drs

   #:admissibility
   #:admissibility-drs

   #:proposition
   #:proposition-ref
   #:proposition-drs

   #:question
   #:question-drs

   #:command
   #:command-drs

   #:drs
   #:drs-domain
   #:drs-conditions))

(defpackage #:race-webservice
  (:use "COMMON-LISP" #:cl-ace-util)
  (:documentation
   "The race-webservice package provides a set of bindings for calling
the RACE Web Service.  The functions defined here try to mimic as
closely as possible the naming conventions of the RACE Web Service,
with only minor renaming to suit Lisp convention.
  The main interface to the RACE Web Service is through the
{defun race-webservice:invoke-race-webservice} and
{defun race-webservice:race} functions.

The former makes requests to the Web Service directly, and returns
results directly from the HTTP client \(DRAKMA [2]\). For instance, to
recreate the example given in the RACE Web Service documentation [1]:
;;; \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
;;; 
;;; <env:Envelope
;;;     xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"
;;;     xmlns:race=\"http://attempto.ifi.uzh.ch/race\">
;;;   <env:Body>
;;;     <race:Reply>
;;;       <race:Runtime>0</race:Runtime>
;;;       <race:Proof>
;;; 	<race:UsedAxioms>
;;; 	  <race:Axiom>1: John is a man.</race:Axiom>
;;; 	  <race:Axiom>3: John is not a man.</race:Axiom>
;;; 	</race:UsedAxioms>
;;; 	<race:UsedAuxAxioms/>
;;;       </race:Proof>
;;;     </race:Reply>
;;;   </env:Body>
;;; </env:Envelope>
;;; \", [more values, see DRAKMA documentation]

The latter parses the response into a CLOS object, specifically, a
{defclass race-webservice:race-reply} . Methods for accessing the
components of the reply follow, for the most part, the naming
convention given in the RACE Web Service documentation [1], and
accompanying WSDL file [3].
* See Also:
- [1] {http://attempto.ifi.uzh.ch/site/docs/race_webservice.html}
- [2] {http://weitz.de/drakma/}
- [3] {http://attempto.ifi.uzh.ch/race_files/race.wsdl} ")
  (:export
   #:+race-webservice-url+

   #:invoke-race-webservice
   #:race

   #:race-reply
   #:race-reply-messages
   #:race-reply-runtime
   #:race-reply-proofs
   #:race-reply-why-not

   #:race-message
   #:race-message-importance
   #:race-message-type
   #:race-message-sentence-id
   #:race-message-subject
   #:race-message-description

   #:race-proof
   #:race-proof-used-axioms
   #:race-proof-used-aux-axioms))
