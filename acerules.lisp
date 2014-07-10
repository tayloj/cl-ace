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

(in-package #:acerules-webservice)

;;;; Calling AceRules

(defconstant +acerules-webservice+
  "http://attempto.ifi.uzh.ch/ws/acerules/acerulesws.perl"
  "The URL of the AceRules webservice:
http://attempto.ifi.uzh.ch/ws/acerules/acerulesws.perl")

(defconstant +acerules-prefix+
  "http://attempto.ifi.uzh.ch/acerules"
  "The AceRules prefix in the SOAP requests and replies:
http://attempto.ifi.uzh.ch/acerules")

(defun invoke-acerules-webservice
       (&rest args &key program mode user-lexicon-url guess
              max-answers rules-output simple-rules-output
              answerset-output answertext-output trace-output
              ace-trace-output (uri +acerules-webservice+)
              (drakma-parameters '(:method :POST)))
  "* Syntax:
invoke-acerules-webservice &rest keys => result
* Arguments and Values:
invoke-acerules-webservice accepts the following keyword arguments,
which are based on those of the webservice.  See the documentation on
the AceRules Webservice [1] for more details.
- :program --- a string of ACE text
- :mode --- a string designator that must be string-equal to one of
\"court\", \"courteous\", \"stable\", \"stable_strong\", or
\"stable-strong\".  This is subtly different from the webservice's
\"court\", \"stable\", and \"stable_strong\", but intended to be more
flexible and Lisp-like, allowing, particularly, the keywords
:courteous, :stable, and :stable-strong.
- :user-lexicon-url --- a string denoting a URL
- :max-answers --- a positive integer
- :guess, :rules-output, :simple-rules-output, :answerset-output,
:answertext-output, :trace-output, :ace-trace-output --- generalized
booleans
invoke-acerules-webservice also accepts two other keyword arguments:
- :uri --- the uri to which the SOAP request is sent. The default
  value is +acerules-webservice+
- :drakma-parameters --- a plist used as arguments to
drakma:http-request.  The default is '\(:method :POST\).  See the
DRAKMA documentation [2] for more details.  Note that
invoke-acerules-webserbservice specifies the content, so a :content
argument in drakma-parameters will be ignored.
* Description:
invoke-acerules-webservice sends a AceRules to URI using
DRAKMA:HTTP-REQUEST.  The values from DRAKMA:HTTP-REQUEST are returned
directly---no post-processing is done.  For post-processing \(e.g.,
parsing into CLOS objects, see {defun acerules-webservice:acerules} .
* See Also:
- {defun acerules-webservice:acerules}
- [1] {http://attempto.ifi.uzh.ch/site/docs/acerules_webservice.html}
- [2] {http://www.weitz.de/drakma/#http-request}"
  (declare (ignore program mode user-lexicon-url guess max-answers
                   rules-output simple-rules-output answerset-output
                   answertext-output trace-output ace-trace-output))
  (remf* args :drakma-parameters :uri)
  (apply 'drakma:http-request uri
         :content (apply 'make-acerules-soap-request args)
         drakma-parameters))

(defun make-acerules-soap-request
       (&key (program nil programp)
             (mode nil modep)
             (user-lexicon-url nil user-lexicon-url-p)
             (guess nil guessp)
             (max-answers nil max-answers-p)
             (rules-output nil rules-output-p)
             (simple-rules-output nil simple-rules-output-p)
             (answerset-output nil answerset-output-p)
             (answertext-output nil answertext-output-p)
             (trace-output nil trace-output-p)
             (ace-trace-output nil ace-trace-output-p))
  "Build a SOAP Envelope and Body, and AceRules request.  Program,
Mode, and UserLexiconURL are the only non-toggle options.  Program is
required, the others are not.  The toggle options are only specified
in the request if they were provided here---this allows the webservice
defaults to be used at the webservice level \(i.e., we don't try to
match them here\)."
  (macrolet ((option-element (element-name var varp)
               `(when ,varp
                  (cxml:with-element* ("ar" ,element-name)
                    (cxml:text (if ,var "on" "off")))))
             (option-elements (&body descriptions)
               `(progn
                  ,@(loop for description in descriptions
                          collecting `(option-element ,@description)))))
    (with-soap-envelope ("env" ("ar" +acerules-prefix+))
      (cxml:with-element* ("ar" "Request")
        ;; Program
        (assert (and programp (stringp program)))
        (cxml:with-element* ("ar" "Program")
          (cxml:text program))
        ;; Mode
        (when modep
          (assert (and (or (stringp mode)
                           (symbolp mode)
                           (characterp mode))
                       (member mode '(court stable stable-strong)
                               :test 'string-equal)))
          (cxml:with-element* ("ar" "Mode")
            (cxml:text
             (pcase mode 'string-equal
               ((courteous)     "court")
               ((stable)        "stable")
               ((stable-strong) "stable_strong")))))
        ;; UserLexiconURL
        (when user-lexicon-url-p
          (assert (stringp user-lexicon-url))
          (cxml:with-element* ("ar" "UserLexiconURL")
            (cxml:text user-lexicon-url)))
        ;; toggle options
        (option-elements
         ("Guess" guess guessp)
         ("MaxAnswers" max-answers max-answers-p)
         ("RulesOutput" rules-output rules-output-p)
         ("SimpleRulesOutput" simple-rules-output simple-rules-output-p)
         ("AnswersetOutput" answerset-output answerset-output-p)
         ("AnswertextOutput" answertext-output answertext-output-p)
         ("TraceOutput" trace-output trace-output-p)
         ("ACETraceOutput" ace-trace-output ace-trace-output-p))))))

(defclass acerules-reply ()
  ((rules
    :type string
    :initarg :rules
    :reader acerules-reply-rules)
   (simple-rules
    :type string
    :initarg :simple-rules
    :reader acerules-reply-simple-rules)
   (answersets
    :type list ; of strings
    :initarg :answersets
    :reader acerules-reply-answersets
    :documentation
    "A list of the string contents of the Answerset elements.")
   (answertexts
    :type list ; of strings
    :initarg :answertexts
    :reader acerules-reply-answertexts
    :documentation
    "A list of the string contents of Answertext elements.")
   (trace
    :type list ; of steps
    :initarg :trace
    :reader acerules-reply-trace
    :documentation
    "A list of step objects.")
   (ace-trace
    :type list ; of steps
    :initarg :ace-trace
    :reader acerules-reply-ace-trace
    :documentation
    "A list of step objects.")))

(defclass step ()
  ((raw
    :type string
    :initarg :raw
    :reader step-raw)
   (delete
    :type string
    :initarg :delete
    :reader step-delete
    :documentation
    "The delete slot is used only for steps in the Trace element, and
     is always used there. It is never used \(and so always unbound\)
     for steps in the ACETrace element.")
   (consistent
    :type string
    :initarg :consistent
    :reader step-consistent)))

(defun acerules (&rest args &key program mode user-lexicon-url guess
                       max-answers rules-output simple-rules-output
                       answerset-output answertext-output trace-output
                       ace-trace-output uri drakma-parameters)
  "* Syntax:
acerules &rest args => result
* Arguments and Values:
- args --- keyword arguments. 
- result --- an {defclass acerules-webservice:acerules-reply}
* Description:
The arguments accepted by acerules are those accepted by
{defun acerules-webservice:invoke-acerules-webservice} .
These are passed through directly to invoke-acerules-webservice.
Unlike invoke-acerules-webservice, which returns its values directly
from DRAKMA:HTTP-REQUEST, acerules parses the reply into corresponding
CLOS classes, viz.: {defclass acerules-webservice:acerules-reply} and
{defclass acerules-webservice:step} .
* Notes:
Since acerules attempts to parse the result of
{defun acerules-webservice:invoke-acerules-webservice} which, in turn,
returns values from DRAKMA:HTTP-REQUEST, drakma-parameters is passed
to {defun acerules-webservice:invoke-acerules-webservice} with
:WANT-STREAM NIL.
* See Also:
- {defun acerules-webservice:invoke-acerules-webservice}
- {defclass acerules-webservice:acerules-reply} "
  (declare (ignore program mode user-lexicon-url guess
                   max-answers rules-output simple-rules-output
                   answerset-output answertext-output trace-output
                   ace-trace-output uri))
  (let* ((reply (apply 'invoke-acerules-webservice
                       :drakma-parameters
                       (list* :want-stream nil drakma-parameters)
                       args))
         (reply (cxml:parse reply (cxml-dom:make-dom-builder))))
    (parse-acerules-reply reply)))

(defun get-acerules-elements (document lname)
  "Retrieve the elements from document named ar:<lname> where ar is an
abbreviation for +acerules-prefix+."
  (dom:get-elements-by-tag-name-ns
   document +acerules-prefix+ lname))

(defun get-acerules-element (document lname)
  "Retrieve the single element from document named ar:<lname> where ar
is an abbreviation for +acerules-prefix+.  If there are no such
elements, or more than one, nil is the primary value, and the
secondary value is a boolean indicating whether there were more than
one such element. \(I.e., if there were none, the second value is NIL,
T otherwise.\)"
  (let ((elements (get-acerules-elements document lname)))
    (if (eql 1 (dom:length elements))
      (values (dom:item elements 0) t)
      (values nil (> (dom:length elements) 1)))))

(defun parse-acerules-reply (reply)
  "* Syntax
parse-acerules-reply reply => acerules-reply
* Arguments and Values:
- reply --- a dom:node representing a Reply element
- acerules-reply --- an acerules-reply object
* Description:
parse-acerules-reply extracts an acerules-reply from the dom:node
representing the ar:Reply element."
  (let ((initargs '())
        (rules (get-acerules-element reply "Rules"))
        (simple-rules (get-acerules-element reply "SimpleRules"))
        (answersets (get-acerules-elements reply "Answerset"))
        (answertexts (get-acerules-elements reply "Answertext"))
        (trace (get-acerules-element reply "Trace"))
        (ace-trace (get-acerules-element reply "ACETrace")))
    (macrolet ((init (var key form)
                 `(unless (null ,var)
                    (setf initargs (list* ,key ,form initargs)))))
      (init rules        :rules        (text-content rules))
      (init simple-rules :simple-rules (text-content simple-rules))
      (init answersets   :answersets   (map-node-list 'text-content answersets))
      (init answertexts  :answertexts  (map-node-list 'text-content answertexts))
      (init trace        :trace        (extract-trace-steps trace))
      (init ace-trace    :ace-trace    (extract-ace-trace-steps ace-trace))
      (apply 'make-instance 'acerules-reply initargs))))

(defun extract-trace-steps (trace)
  "Extract a list of steps from a dom:node representing an ar:Trace
element.  steps in ar:Trace elements have Raw, Delete, and Consistent
elements, each of which has string content."
  (flet ((make-step (step)
           (make-instance 'step
            :raw (text-content (get-acerules-element step "Raw"))
            :delete (text-content (get-acerules-element step "Delete"))
            :consistent (text-content (get-acerules-element step "Consistent")))))
    (map-element-nodes #'make-step (dom:child-nodes trace))))

(defun extract-ace-trace-steps (trace)
  "Extract a list of steps from a dom:node representing an ar:ACETrace
element.  steps in ar:ACETrace elements have Raw and Consistent \(but
not Delete\) elements, both of which have string content."
  (flet ((make-step (step)
           (make-instance 'step
            :raw (text-content (get-acerules-element step "Raw"))
            :consistent (text-content (get-acerules-element step "Consistent")))))
    (map-element-nodes #'make-step (dom:child-nodes trace))))

     
  


