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

(in-package #:race-webservice)

(defconstant +race-webservice-url+
  "http://attempto.ifi.uzh.ch/ws/race/racews.perl"
  "The URL of the RACE webservice.")

(defconstant +race-prefix-uri+
  "http://attempto.ifi.uzh.ch/race")

(defun invoke-race-webservice (&rest args
                                     &key axioms
                                     theorems
                                     mode
                                     si st ot sdt dodt iodt sti aux
                                     (uri +race-webservice-url+)
                                     (drakma-parameters '()))
  "* Syntax:
invoke-race-webservice &key axioms theorems mode
                            si st or sdt dodt iodt sti aux
                            drakma-parameters uri => result*
* Arguments and Values:
- axioms --- a string
- theorems --- a string or nil
- mode --- one of the symbols :check-consistency, :prove, :answer-query
- si, st, ot, sdt, dodt, iodt, sti, aux --- generalized booleans
- uri --- a string or PURI:URI object, defaults to +race-webservice-url+
- drakma-parameters --- a plist, passed to DRAKMA:HTTP-REQUEST
- results --- results of DRAKMA:HTTP-REQUEST
* Description:
invoke-race-webservice invokes the RACE Webservice by sending a SOAP
request to URI.  More documentation on the RACE Webservice is available [1].
* See Also:
- [1] {http://attempto.ifi.uzh.ch/site/docs/race_webservice.html}"
  (declare (ignore axioms theorems mode si st ot sdt dodt iodt sti aux))
  (remf* args :uri :drakma-parameters)
  (apply 'drakma:http-request uri
         :content (apply 'make-race-soap-request args)
         drakma-parameters))

(defun make-race-soap-request
       (&key axioms theorems mode si st ot sdt dodt iodt sti aux)
  (assert (stringp axioms) (axioms)
    "Axioms must be a string of ACE text, but was ~S." axioms)
  (assert (or (and (member mode '("prove" "answer-query" "answer_query")
                           :test 'string-equal)
                   (stringp theorems))
              (and (member mode '("check-consistency" "check_consistency")
                           :test 'string-equal)
                   (null theorems)))
      (mode theorems)
    "Mode must be a string designator string-equal to prove, ~
     answer-query \(or answer_query\), or check-consistency ~
     \(or check_consistency\).  If mode is prove or answer-query, ~
     then theorems much be a string.  If mode is check-consistency, ~
     then theorems must be null.  Mode was ~S.  Theorems was ~S."
    mode theorems)
  (with-soap-envelope ("env" ("race" +race-prefix-uri+))
    (cxml:with-element* ("race" "Request")
      ;; Axioms
      (cxml:with-element* ("race" "Axioms")
        (cxml:text axioms))
      ;; Theorems
      (unless (null theorems)
        (cxml:with-element* ("race" "Theorems")
          (cxml:text theorems)))
      ;; Mode
      (cxml:with-element* ("race" "Mode")
        (cxml:text
         (pcase mode #'string-equal
           (("prove")
            "prove")
           (("answer-query" "answer_query")
            "answer_query")
           (("check-consistency" "check_consistency")
            "check_consistency"))))
      ;; Parameters
      (loop for p in '("si" "st" "ot" "sdt" "dodt" "iodt" "sti" "aux")
            for pp in (list si st ot sdt dodt iodt sti aux)
            when pp do (cxml:with-element* ("race" "Parameter")
                         (cxml:text p))))))

(defun race (&rest args
                   &key
                   axioms theorems mode
                   si st ot sdt dodt iodt sti aux
                   (uri +race-webservice-url+)
                   (drakma-parameters '())
                   (signal-warning-messages t)
                   (signal-error-messages nil))
  "* Syntax:
race &rest args &key signal-warning-messags signal-error-messages => reply
* Arguments and Values:
- args --- a plist of args for {defun race-webservice:invoke-race-webservice}
- signal-warning-messages, signal-error-messages --- booleans,
defaults are t and nil respectively
- reply --- a {defclass race-webservice:race-reply} object
* Description:
Race applies {defun race-webservice:invoke-race-webservice} with args,
parses the reply, and signals warnings and errors depending on the
values of signal-warning-messages and signal-error messages.
  RACE messages that have importance type \"warning\" will, by default
be signalled, while messages with importance \"error\" will not.
* Notes:
Since CXML:PARSE may or may not be able to handle a stream returned by
DRAKMA:HTTP-REQUEST, any drakma-parameters is passed to
{defun race-webservice:invoke-race-webservice} with :want-stream nil.
  invoke-race-webservice is called with :allow-other-keys t \(so that
signal-warning-messages and signal-error-messsages can passed
through\) and as a result, arguments that would cause an unexpected
keyword error to be signalled will not do so."
  (declare (ignore axioms theorems mode si st ot
                   sdt dodt iodt sti aux uri))
  (remf* args :signal-warning-messages :signal-error-messages)
  (let ((reply (parse-race-output
                (apply' invoke-race-webservice
                      :drakma-parameters (list* :want-stream nil
                                                drakma-parameters)
                      args))))
    (prog1 reply
      (signal-race-reply-messages reply
       :signal-warning-messages signal-warning-messages
       :signal-error-messages signal-error-messages))))

(defun signal-race-reply-messages
       (reply &key signal-warning-messages signal-error-messages)
  (dolist (message (race-reply-messages reply))
    (etypecase message
      (race-error-message
       (when signal-error-messages
         (error message)))
      (race-warning-message
       (when signal-warning-messages
         (warn message))))))

;;;; Classes related to the RACE Reply

(defclass race-reply ()
  ((messages
    :type list
    :reader race-reply-messages
    :initform '()
    :initarg :messages)
   (runtime
    :type integer
    :reader race-reply-runtime
    :initarg :runtime)
   (proofs
    :type list 
    :reader race-reply-proofs
    :initform '()
    :initarg :proofs)
   (why-not
    :type list ; of strings
    :reader race-reply-why-not
    :initform '()
    :initarg :why-not
    :documentation
    "A list of strings, the content of the race:Word elements that
     were in the race:WhyNot element.")))

(define-condition race-message (condition)
  ((importance
    :type (member :warning :error)
    :reader race-message-importance
    :documentation "One of the symbols :warning, or :error.")
   (type
    :type string
    :reader race-message-type
    :initarg :type)
   (sentence-id
    :type (or null integer)
    :reader race-message-sentence-id
    :initarg :sentence-id
    :documentation "An integer \(or left unbound\).")
   (subject
    :type string
    :reader race-message-subject
    :initarg :subject)
   (description
    :type string
    :reader race-message-description
    :initarg :description))
  (:report report-race-message))

(defun report-race-message (condition stream)
  (format stream
          "~A ~A (This is a ~S message~@[, see sentence ~S~].)"
          (race-message-subject condition)
          (race-message-description condition)
          (race-message-type condition)
          (if (not (slot-boundp condition 'sentence-id)) nil
            (race-message-sentence-id condition))))

(define-condition race-warning-message (warning race-message)
  ((importance :initform :warning)))

(define-condition race-error-message (error race-message)
  ((importance :initform :error)))

(defclass race-proof ()
  ((used-axioms
    :type list
    :reader race-proof-used-axioms
    :initarg :used-axioms)
   (used-aux-axioms
    :type list
    :reader race-proof-used-aux-axioms
    :initarg :used-aux-axioms)))

;;;; Parsing the RACE Reply

;;; Some utility XML handling functions

(defun get-race-elements-by-tag-name (document tag-name)
  (dom:get-elements-by-tag-name-ns
   document +race-prefix-uri+ tag-name))

(defun get-race-element-by-tag-name (document tag-name)
  (let ((elements (get-race-elements-by-tag-name document tag-name)))
    (if (eql 0 (dom:length elements)) nil
      (dom:item elements 0))))

(defun element-text-content (element)
  (case (dom:length (dom:child-nodes element))
    (0 "")
    (1 (dom:data (dom:first-child element)))
    (t (error "Don't know how to extract text content from element ~
               ~S with more than one child." element))))

(defun mapnodes (fn nodelist &aux (results '()))
  (dom:do-node-list (x nodelist (nreverse results))
    (push (funcall fn x) results)))

;;; Parsing functions proper

(defun parse-race-output (blob)
  "* Arguments and Values:
- blob --- an XML blob that contains the SOAP envelope"
  (let* ((response (cxml:parse blob (cxml-dom:make-dom-builder)))
         (messages (get-race-elements-by-tag-name response "Message"))
         (runtime (get-race-element-by-tag-name response "Runtime"))
         (proofs (get-race-elements-by-tag-name response "Proof"))
         (words (get-race-elements-by-tag-name response "Word")))
    (make-instance 'race-reply
     :proofs (extract-proofs proofs)
     :messages (extract-messages messages)
     :runtime (extract-runtime runtime)
     :why-not (extract-words words))))

(defun extract-proofs (elements &aux (proofs '()))
  "* Arguments and Values:
- elements --- a node list of race:Proof elements"
  (dom:do-node-list (element elements (nreverse proofs))
    (push (make-instance 'race-proof
           :used-axioms     (mapnodes 'element-text-content
                                      (get-race-elements-by-tag-name
                                       element "Axiom"))
           :used-aux-axioms (mapnodes 'element-text-content
                                      (get-race-elements-by-tag-name
                                       element "AuxAxiom")))
          proofs)))

(defun extract-messages (elements &aux (messages '()))
  "* Arguments and Values:
- elements --- a node list of race:Message element nodes"
  (dom:do-node-list (element elements (nreverse messages))
    (push (extract-message element) messages)))

(defun extract-message (element)
  "* Arguments and Values:
- element --- a race:Message element node"
  (flet ((et (name)
           (element-text-content
            (get-race-element-by-tag-name
             element name))))
    (apply 'make-condition (pcase (et "Importance") #'string=
                             ("warning" 'race-warning-message)
                             ("error"   'race-error-message))
           :type (et "Type")
           :subject (et "Subject")
           :description (et "Description")
           (unless (string= "" (et "SentenceID"))
             (list :sentence-id (parse-integer (et "SentenceID")))))))

(defun extract-runtime (element)
  "* Arguments and Values:
- element --- an race:Runtime element node"
  (parse-integer (element-text-content element)))

(defun extract-words (elements)
  "* Arguments and Values:
- elements --- a node list of race:Word element nodes"
  (mapnodes 'element-text-content elements))
