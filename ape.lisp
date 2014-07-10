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

(in-package #:ape-webservice)

;;;; Calling APE

(defconstant +ape-webservice-url+
  "http://attempto.ifi.uzh.ch/ws/ape/apews.perl"
  "The URL of the APE webservice. This has the value
{http://attempto.ifi.uzh.ch/ws/ape/apews.perl} .")

(defun solo-parameter-p (object)
  "solo-parameter-p object object => generalized-boolean
object---an object
Solo-parameter-p returns true is object is a string designator and the
string-downcased deisngated string is one of the valid parameters that
can be passed to the APE Webservice, namely: drs, drsxml, drspp,
drshtml, paraphrase, paraphrase1, paraphrase2, tokens, sentences,
syntax, syntaxpp, syntaxd, syntaxdpp, owlfss, owlfsspp, owlrdf,
owlxml, fol, pnf."
  (and (or (characterp object)
           (stringp object)
           (symbolp object))
       (find (string-downcase object)
             #("drs" "drsxml" "drspp" "drshtml"
                     "paraphrase" "paraphrase1" "paraphrase2"
                     "tokens" "sentences" "syntax" "syntaxpp"
                     "syntaxd" "syntaxdpp" "owlfss" "owlfssp"
                     "owlrdf" "owlxml" "fol" "pnf"
                     "ruleml"
                     "tptp")
             :test 'string=)))

(defun invoke-ape-webservice
       (&key
        (text nil text-p) (file nil file-p)
        (ulextext nil ulextext-p) (ulexfile nil ulexfile-p)
        (ulexreload nil ulexreload-p)
        (cdrs nil cdrs-p) (cdrsxml nil cdrsxml-p)
        (cdrspp nil cdrspp-p) (cdrshtml nil cdrshtml-p)
        (cparaphrase nil cparaphrase-p)
        (cparaphrase1 nil cparaphrase1-p)
        (cparaphrase2 nil cparaphrase2-p)
        (ctokens nil ctokens-p) (csentences nil csentences-p)
        (csyntax nil csyntax-p) (csyntaxpp nil csyntaxpp-p)
        (csyntaxd nil csyntaxd-p) (csyntaxdpp nil csyntaxdpp-p)
        (cowlfss nil cowlfss-p) (cowlfsspp nil cowlfsspp-p)
        (cowlrdf nil cowlrdf-p) (cowlxml nil cowlxml-p)
        (cfol nil cfol-p) (cpnf nil cpnf-p)
        (solo nil solo-p) (uri nil uri-p)
        (noclex nil noclex-p) (guess nil guess-p)
        ;; See note above about ruleml
        (cruleml nil cruleml-p)
        (ctptp nil ctptp-p)
        (url +ape-webservice-url+)
        (drakma-arguments '(:method :post)))
  "* Syntax:
invoke-ace-webservice &rest args => result*
* Arguments and Values:
- args --- a plist of arguments
- url --- a string or PURI:URI object, default
is {defconstant +ape-webservice-url+} 
- drakma-arguments --- a plist, defaults to '(:method :post)
- results --- results from drakma:http-request
* Description:
Most of the keyword arguments are derived from (and share names) with
the APE Webservice [1].  Arguments with on|off values are boolean
flags, each generalized booleans.  Arguments which accept text or
URL/URIs accepts strings.  The solo argument should be a string
designator, and its value is string-downcased before being passed to
the webservice.  url is the URL to which the request will be sent. The
value of drakma-arguments should be a plist, and will be passed to
drakma:http-request. drakma-arguments may contain any arguments that
drakma:http-request accepts, but invoke-ape-webservice builds and
passes a :parameters argument given before drakma-arguments, so the
:parameters argument will be ignored.
  The results are returned directly from drakma:http-request [2]. The
url for the request is the value of +ape-webservice-url+, the method
is :post, and the parameters are constructed based on args.  All other
arguments assume their default values.
* Notes:
The cruleml option is supported by the websevice, and the XML Schema
specification for APE output [3] includes a ruleml field. However, the APE
Webservice Specification [1] does not actually mention the cruleml option.
See the the message Attempto mailing list message \"RuleML question\"
[4] for more information.
  The value of drakma-arguments has a default value '(:method :post),
since the queries, depending on the options used, can grow to be
rather large. Thus, when specifying a drakma-arguments argument, it
may be wise to include :method :post.
* See Also:
- [1] {http://attempto.ifi.uzh.ch/site/docs/ape_webservice.html}
- [2] {http://www.weitz.de/drakma/#http-request}
- [3] {http://attempto.ifi.uzh.ch/site/docs/XMLSchema/apews_output.xsd}
- [4] {https://lists.ifi.uzh.ch/pipermail/attempto/2009-February/000137.html}"
  (macrolet
      ((value-options (&rest vars)
         `(nconc
           ,@(loop
              for var in vars
              collect `(if (not ,(intern (strcat var '#:-p))) '()
                         (list (cons ,(string-downcase var) ,var))))))
       (toggle-options (&rest vars)
         `(nconc
           ,@(loop
              for var in vars
              collect `(if (not ,(intern (strcat var '#:-p))) '()
                         (list (cons ,(string-downcase var) (if ,var "on" "off"))))))))
    (apply 'drakma:http-request
           url
           :parameters
           (nconc
            (when solo-p
              (assert (solo-parameter-p solo) (solo))
              `(("solo" . ,(string-downcase solo))))
            (value-options text file ulextext ulexfile uri)
            (toggle-options ulexreload cdrs cdrsxml cdrspp cdrshtml
                            cparaphrase cparaphrase1 cparaphrase2 ctokens
                            csentences csyntax csyntaxpp csyntaxd csyntaxdpp
                            cowlfss cowlfsspp cowlrdf cowlxml cfol cpnf
                            noclex guess
                            cruleml
                            ctptp))
           drakma-arguments)))

;;;; APE Result Data Structures

(define-condition ape-condition (condition) ()
  (:documentation "The type of conditions created, and possibly
signalled, by the functions relating to the ape-webservice. The direct
subtypes of ape-condition are
{define-condition ape-webservice:ape-error} \(representing failures in
the APE webservice\), and
{define-condition ape-webservice:message} \(representing the messages of
{defclass ape-webservice:ape-result} objects\)."))

(deftype ape-error-type ()
  "Objects which are ace-error types are the nine keyword symbols,
:fetch-acetext, :fetch-lexicon, :socket-init, :socket-connect,
:socket-close, :logfile-open, :logfile-close, :ulex-open, and
:ulex-close. Each keyword xxx-yyy represents corresponds to the string
\"ace xxx yyy\" that is a possible value for the type attribute of an
ape-error-object."
  '(member :fetch-acetext :fetch-lexicon
           :socket-init :socket-connect :socket-close
           :logfile-open :logfile-close
           :ulex-open :ulex-close))
  
(define-condition ape-error (error ape-condition)
  ((type
    :type ape-error-type
    :reader ape-error-type
    :documentation "An object of type ape-error-type, that is one of
the keywords :fetch-acetext, :fetch-lexicon, :socket-init,
:socket-connect, :socket-close, :logfile-open, :logfile-close,
:ulex-open, and :ulex-close."))
  (:report (lambda (condition stream)
             (format stream
                     "The APE webservice failed with an error of type ~S."
                     (ape-error-type condition))))
  (:documentation "The parent type of ape-errors---those that occur
when the APE webservice returns a single error element as output. The
type of the error can be extracted using ape-error-type. The actual
condition types that are used are child type of ape-error. They are
- {define-condition ape-webservice:fetch-acetext-error}
- {define-condition ape-webservice:fetch-lexicon-error}
- {define-condition ape-webservice:socket-init-error}
- {define-condition ape-webservice:socket-connect-error}
- {define-condition ape-webservice:socket-close-error}
- {define-condition ape-webservice:logfile-open-error}
- {define-condition ape-webservice:logfile-close-error}
- {define-condition ape-webservice:ulex-open-error}
- {define-condition ape-webservice:ulex-close-error}"))

(defmacro define-ape-error (name error-type)
  "* Syntax
define-ape-error name error-type
* Arguments and Values:
- name --- a symbol, not evaluated
- error-type --- a keyword of type {deftype ape-error-type}
* Description:
define-ape-error defines a condition that has
{define-condition ape-webservice::ape-error} as a parent condition,
and has error-type as an initform for the type slot."
  `(define-condition ,name (ape-error)
     ((type
       :initform ,error-type
       :documentation ,(format nil "The value of this slot is ~S. "
                               error-type)))))

#+:lispworks
(progn 
  (dspec:define-form-parser (define-ape-error (:alias define-condition)))
  (dspec:define-dspec-alias define-ape-error (name)
    `(define-condition ,name)))

(define-ape-error fetch-acetext-error  :fetch-acetext)
(define-ape-error fetch-lexicon-error  :fetch-lexicon)
(define-ape-error socket-init-error    :socket-init)
(define-ape-error socket-connect-error :socket-connect)
(define-ape-error socket-close-error   :socket-close)
(define-ape-error logfile-open-error   :logfile-open)
(define-ape-error logfile-close-error  :logfile-close)
(define-ape-error ulex-open-error      :ulex-open)
(define-ape-error ulex-close-error     :ulex-close)

(defclass ape-result ()
  ((duration
    :type duration
    :reader ape-result-duration
    :initarg :duration)
   (input
    :type string
    :reader ape-result-input
    :initarg :input)
   (tokens
    :type string
    :reader ape-result-tokens
    :initarg :tokens)
   (sentences
    :type string
    :reader ape-result-sentences
    :initarg :sentences)
   (drs
    :type string
    :reader ape-result-drs
    :initarg :drs)
   (syntax
    :type string
    :reader ape-result-syntax
    :initarg :syntax)
   (syntaxpp
    :type string
    :reader ape-result-syntaxpp
    :initarg :syntaxpp)
   (syntaxd
    :type string
    :reader ape-result-syntaxd
    :initarg :syntaxd)
   (syntaxdpp
    :type string
    :reader ape-result-syntaxdpp
    :initarg :syntaxdpp)
   (drspp
    :type string
    :reader ape-result-drspp
    :initarg :drspp)
   (drsxml
    :type string
    :reader ape-result-drsxml
    :initarg :drsxml)
   (drshtml
    :type string
    :reader ape-result-drshtml
    :initarg :drshtml)
   (paraphrase
    :type string
    :reader ape-result-paraphrase
    :initarg :paraphrase)
   (paraphrase1
    :type string
    :reader ape-result-paraphrase1
    :initarg :paraphrase1)
   (paraphrase2
    :type string
    :reader ape-result-paraphrase2
    :initarg :paraphrase2)
   (owlrdf
    :type string
    :reader ape-result-owlrdf
    :initarg :owlrdf)
   (owlfss
    :type string
    :reader ape-result-owlfss
    :initarg :owlfss)
   (owlfsspp
    :type string
    :reader ape-result-owlfsspp
    :initarg :owlfsspp)
   (owlxml
    :type string
    :reader ape-result-owlxml
    :initarg :owlxml)
   (ruleml
    :type string
    :reader ape-result-ruleml
    :initarg :ruleml)
   (fol
    :type string
    :reader ape-result-fol
    :initarg :fol)
   (pnf
    :type string
    :reader ape-result-pnf
    :initarg :pnf)
   (tptp
    :type string
    :reader ape-result-tptp
    :initarg :tptp)
   (messages
    :initform '()
    :type list ; of messages
    :reader ape-result-messages
    :initarg :messages
    :documentation
    "The value of this slot is a list of message objects.")
   (timestamp
    :type string ; an xsd:date 
    :reader ape-result-timestamp
    :initarg :timestamp
    :documentation
    "The value of this slot is an xsd:date")))

(defclass duration ()
  ((tokenizer
    :type float
    :reader duration-tokenizer
    :initarg :tokenizer)
   (parser
    :type float
    :reader duration-parser
    :initarg :parser)
   (refres
    :type float
    :reader duration-refres
    :initarg :refres)))

(deftype message-importance ()
  "Objects that are message-importances are the keywords :error and
:warning, corresponding to the strings \"error\" and \"warning\" which
may be the values of the importance attribute of message elements."
  `(member :error :warning))

(deftype message-type ()
  "Objects that are message-types are the keywords :character, :word,
:sentence, :syntax, :anaphor, :timelimit, :lexicon, :owl, and :ape,
which corresponding to the strings may be the values of the type
attribute of message elements."
  `(member :character :word :sentence
           :syntax :anaphor :timelimit
           :lexicon :owl :ape))

(define-condition message (ape-condition)
  ((importance
    :type message-importance
    :reader message-importance)
   (type
    :type message-type
    :reader message-type)
   (sentence
    :type (or string (integer 1))
    :reader message-sentence
    :initarg :sentence)
   (token
    :type (or string (integer 1))
    :reader message-token
    :initarg :token)
   (value
    :type string
    :reader message-value
    :initarg :value)
   (repair
    :type string
    :reader message-repair
    :initarg :repair))
  (:report ape-message-reporter)
  (:documentation "The message condition type is a parent-type of both
the {define-condition ape-webservice:ape-warning-message} and
{define-condition ape-webservice:ape-error-message} conditions types.
The readers defined for the message condition should be used for
accessing the slots of any instances \(and the APE Output
Specification defines only the message element\).  The specialized
child condition types are used to effect the default behavior for
errors and warnings when the objects are signalled."))

(defun ape-message-reporter (condition stream)
  (format stream " Type: ~S~@[, Sentence: ~S~]~@[, Token: ~S~], Value: ~S. ~A"
          (message-type condition)
          (message-sentence condition)
          (message-token condition)
          (message-value condition)
          (message-repair condition)))

;;; Message Types

(defmacro define-typed-message-conditions (types)
  "For each type in types, defines a condition by
;;; (define-condition <type>-message (message)
;;;   ((type :initform <type>)))"
  (let ((-msg (string '#:-message)))
    `(#-:lispworks progn
      #+:lispworks dspec:def
      #+:lispworks (define-typed-message-conditions ,types)
      ,@(loop
         for type in types
         for namestring = (concatenate 'string (string type) -msg)
         for name = (intern namestring)
         collecting
         `(define-condition ,name (message)
            ((type
              :initform ,type
              :documentation
              ,(format nil "The value of this slot is ~S." type))))))))

(define-typed-message-conditions (:character :word :sentence
                                  :syntax :anaphor :timelimit
                                  :lexicon :owl :ape))

;;; Message Importances

(defmacro define-important-message-conditions (importances)
  "For each imp in importances, defines a condition by
;;; (define-condition <imp>-message (<imp> message)
;;;   ((importance :initform <imp>)))"
  (let ((-msg (string '#:-message)))
    `(#-:lispworks progn
      #+:lispworks dspec:def
      #+:lispworks (define-important-message-conditions ,importances)
       ,@(loop for importance in importances
               for namestring = (concatenate 'string (string importance) -msg)
               for name = (intern namestring)
               for kind = (ecase importance
                            (:error 'error)
                            (:warning 'warning))
               collecting
               `(define-condition ,name (message ,kind)
                  ((importance
                    :initform ,importance
                    :documentation
                    ,(format nil "The value of this slot is ~S."
                             importance))))))))

(define-important-message-conditions
 (:warning :error))

;;; The concrete message types

(defmacro define-typed-important-message-conditions (types importances)
  "For each pair (<t>,<i>) where <t> is in types and <i> is in
importances, defines a condition ape-<t>-<i>-message by
;;; (define-condition ape-<t>-<i>-message (<t>-message <i>-message) ())"
  (let ((ape- (string '#:ape-))
        (-msg (string '#:-message)))
    `(#-:lispworks progn
      #+:lispworks dspec:def
      ;; this is a bit fragile, since it only takes types into account
      #+:lispworks (define-typed-important-message-conditions ,types)
       ,@(loop for ty in types
               for type = (string ty)
               nconcing
               (loop for imp in importances
                     for importance = (string imp)
                     collecting
                     `(define-condition
                          ,(intern (concatenate 'string ape- type "-" importance -msg))
                          (,(intern (concatenate 'string type -msg))
                           ,(intern (concatenate 'string importance -msg)))
                        ()))))))

(define-typed-important-message-conditions
 (:character :word :sentence
  :syntax :anaphor :timelimit
  :lexicon :owl :ape)
 (:warning :error))

;;;; Parsing APE Results

(defun ape (&rest args
                  &key (solo nil solop)
                  (signal-warning-messages t)
                  (signal-error-messages nil)
                  &allow-other-keys)
  "* Syntax:
ape &rest args &key signal-ape-warnings signal-ape-errors => result
* Arguments and Values:
- args --- arguments to {defun ape-webservice:invoke-ape-webservice}
- signal-warnings-messages, signal-error-messages --- generalized
booleans, defaulting to t and nil, respectively
- result --- an {defclass ape-webservice:ape-result}
or {define-condition ape-webservice:ape-error}
* Description:
Ape invokes the APE-webservice using
{defun ape-webservice:invoke-ape-webservice} (to which args are passed
directly), but differs from {defun ape-webservice:invoke-ape-webservice}
in that the resulting {defclass ape-webservice:ape-result} or
{defclass\ ape-webservice:ape-error} is parsed into lisp objects, and
appropriate conversions to lisp data structures are performed where
possible and supported. When the solo parameter is provided, \(even
when nil\) no parsing or conversion is done, and ape behaves as is the
same calling {defun ape-webservice:invoke-ape-webservice} .
  When signal-warning-messages is non-nil (the default), each
{define-condition ape-webservice:message} whose
importance is :warning is signalled using WARN. When
signal-error-messages is non-nil, each message whose importance is
:error is signalled with ERROR. These flags affect only the messages
of an {defclass ape-webservice:ape-result} . Every
{define-condition ape-webservice:ape-error} produced by the webservice
is signalled with ERROR.
  The conversions currently supported are as follows:
- All ape-result and error XML outputs are cast into corresponding
CLOS classes (and all with the same name as their ACE counterpart,
with the single exception of error, which becomes
{define-condition ape-webservice:ape-error} ).
- The messages slot of the {defclass ape-webservice:ape-result}
object is a list of messages.
- Floating point values in duration objects are parsed from XML
attribute strings.
- Enumerated string values are recast as keyword symbols, usually with
the same name (with appropriate case conversions). The various failure
strings, however, have a bit more processing. E.g., \"fail fetch
acetext\" become :fetch-acetext.
- The most significant processing to lisp objects is the parsing of
the DRSXML output to the {defclass ace-drs:drs} objects \(whose API is
exported from the ACE-DRS package\).
* Notes:
Note that not all messages with type error indicate that call to the
webservice was completely unsuccessful. For instance, the sentence
\"John believes that Mary likes Bill.\" is valid ACE, but will cause
an error message to be generated if cowlfss is also non-nil, since
sentence subordination is not supported in OWL. This is the rationale
for signalling warnings, but not errors, by default."
  (declare (ignore solo))
  ;; invoke-ape-webservice doesn't take :signal-*-messages args
  (remf* args :signal-warning-messages :signal-error-messages)
  (flet ((prompt-for-line* (message &rest args)
           (list (apply 'prompt-for-line message args)))
         (invoke-ape ()
           "do-ape wraps the call to ape, and the (possible) parsing
            of its output so that it's easier to call again from the
            restarts that appear below."
           (let ((output (apply 'invoke-ape-webservice args)))
             (if solop output
               (let ((ape-result (parse-ape-output output)))
                 (prog1 ape-result
                   (etypecase ape-result
                     (ape-error  (error ape-result))
                     (ape-result (signal-ape-messages
                                  ape-result
                                  :errors signal-error-messages
                                  :warnings signal-warning-messages)))))))))
    ;; We just keep trying to return the value from (invoke-ape).  A
    ;; few restarts are available.  Each modifies args in some
    ;; appropriate way.  When the restart finishes, we try it all over
    ;; again.
    (loop
     (restart-case (return (invoke-ape))
       (discard-owl ()
         :report "Discard OWL related arguments ~
                 \(:cowlfss, :cowlfsspp, :cowlrdf, and :cowlxml\)."
         :test (lambda (condition) (typep condition 'owl-message))
         (remf* args :cowlfss :cowlfsspp :cowlrdf :cowlxml))
       (use-file (file)
         :report "Supply different ACE text through a URL."
         :interactive (lambda () (prompt-for-line* "Enter a new file URL:"))
         (setf args (list* :file file args)))
       (use-text (text)
         :report "Supply different ACE text as a string."
         :interactive (lambda () (prompt-for-line* "Enter new ACE text:"))
         (setf args (list* :text text (remf* args :file))))
       (use-ulexfile (ulexfile)
         :report "Supply different lexicon through a URL."
         :interactive (lambda () (prompt-for-line* "Enter a new lexicon URL:"))
         (setf args (list* :ulexfile ulexfile args)))
       (use-ulex (ulex)
         :report "Supply different lexicon as a string."
         :interactive (lambda () (prompt-for-line* "Enter new lexicon text:"))
         (setf args (list* :ulex ulex (remf* args :ulexfile))))))))

;; Restart functions for APE

(define-restart-function discard-owl ()
  "* Syntax:
discard-owl &optional condition => nil
* Description:
Invokes the most recently applicable restart named discard-owl.")

(define-restart-function use-file (file)
  "* Syntax:
use-file file &optional condition => nil
* Arguments and Values:
- file --- a string denoting a URL
- condition --- a condition
* Description:
Invokes the most recently applicable restart named use-file.")

(define-restart-function use-text (text)
  "* Syntax:
use-text text &optional condition => nil
* Arguments and Values:
- text --- a string of ACE text
- condition --- a condition
* Description:
Invokes the most recently applicable restart named use-text.")
  
(define-restart-function use-ulexfile (ulexfile)
  "* Syntax:
use-ulexfile ulexfile &optional condition => nil
* Arguments and Values:
- ulexfile --- a string denoting a URL
- condition --- a condition
* Description:
Invokes the most recently applicable restart named use-ulexfile.")
  
(define-restart-function use-ulex (ulex)
  "* Syntax:
use-ulexfile ulexfile &optional condition => nil
* Arguments and Values:
- ulexfile --- a string of lexicon text
- condition --- a condition
* Description:
Invokes the most recently applicable restart named use-ulex.")

(defun signal-ape-messages (ape-result &key errors warnings)
  "* Syntax:
signal-ape-messages ape-result &key errors warnings => \|
* Arguments and Values:
- ape-result --- an {defclass ape-webservice:ape-result} object
- errors, warnings --- generalized booleans, default for each is nil
* Description:
Signal-ape-messages signals messages \(which are conditions\) in the
ape-result. If errors is true, then each
{define-condition ape-webservice:ape-error-message} is signalled with
ERROR. If warnings is true, then each
{define-condition ape-webservice:ape-warning-message} is signalled
with WARN."
  (dolist (message (ape-result-messages ape-result) (values))
    (etypecase message
      (error   (when errors   (error message)))
      (warning (when warnings (warn  message))))))

(defun parse-ape-output (blob)
  "* Syntax:
parse-ape-output blob => result
* Arguments and Values:
- blob --- a string
- result --- an {defclass ape-webservice:ape-error} or
{defclass ape-webservice:ape-result} object
* Description:
blob is a string containing the output from the APE webservice, when
the solo attribute has not been specified, that is, a result conforming to
{http://attempto.ifi.uzh.ch/site/docs/XMLSchema/apews_output.xsd} ."
  (let* ((document (cxml:parse blob (cxml-dom:make-dom-builder)))
         (root (dom:document-element document)))
    (pcase (dom:tag-name root) #'string=
      ("error"     (extract-ape-error-from-node root))
      ("apeResult" (extract-ape-result-from-node root)))))

(defun extract-ape-error-from-node (node)
  (make-condition 
   (pcase (dom:get-attribute node "type") #'string=
     ("fail fetch acetext"  'fetch-acetext-error)
     ("fail fetch lexicon"  'fetch-lexicon-error)
     ("fail socket init"    'socket-init-error)
     ("fail socket connect" 'socket-connect-error)
     ("fail socket close"   'socket-close-error)
     ("fail logfile open"   'logfile-open-error)
     ("fail logfile close"  'logfile-close-error)
     ("fail ulex open"      'ulex-open-error)
     ("fail ulex close"     'ulex-close-error))))

(defun extract-ape-result-from-node (node &aux (initargs '()))
  "* Description:
extract-ape-result-from-node returns an ape-result constructed from
the DOM element node."
  ;; initargs is a list of initargs that will be used to construct an
  ;; ape-result.  initargs will be built from the attributes and child
  ;; elements of the node.  The local function (init ind val) adds
  ;; ind/val to the initargs, and ape-result invokes make-instance.
  (flet ((init (ind val) (setf (getf initargs ind) val))
         (ape-result () (apply 'make-instance 'ape-result initargs)))
    ;; timestamp is the only ape-result slot value that is based on an
    ;; attribute value...
    (when (dom:has-attribute node "timestamp")
      ;; FIXME: This is an XML Schema date. We could
      ;; probably parse it into a universal time.
      (init :timestamp (dom:get-attribute node "timestamp")))
    ;; ...the rest are all extracted from child elements.  Note that
    ;; the return value of the do-element-nodes is (ape-result), since
    ;; after iterating through the child nodes, there are no more
    ;; initargs to be processed.
    (do-element-nodes (child (dom:child-nodes node) (ape-result))
      (pcase (dom:tag-name child) #'string=
        ("duration"    (init :duration    (extract-duration-from-node child)))
        ("input"       (init :input       (text-content child)))
        ("tokens"      (init :tokens      (text-content child)))
        ("sentences"   (init :sentences   (text-content child)))
        ("drs"         (init :drs         (text-content child)))
        ("drsxml"      (init :drsxml      (ace-drs:parse-drsxml
                                           (text-content child))))
        ("syntax"      (init :syntax      (text-content child)))
        ("syntaxpp"    (init :syntaxpp    (text-content child)))
        ("syntaxd"     (init :syntaxd     (text-content child)))
        ("syntaxdpp"   (init :syntaxdpp   (text-content child)))
        ("drspp"       (init :drspp       (text-content child)))
        ("drshtml"     (init :drshtml     (text-content child)))
        ("paraphrase"  (init :paraphrase  (text-content child)))
        ("paraphrase1" (init :paraphrase1 (text-content child)))
        ("paraphrase2" (init :paraphrase2 (text-content child)))
        ("owlrdf"      (init :owlrdf      (text-content child)))
        ("owlfss"      (init :owlfss      (text-content child)))
        ("owlfsspp"    (init :owlfsspp    (text-content child)))
        ("owlxml"      (init :owlxml      (text-content child)))
        ("ruleml"      (init :ruleml      (text-content child)))
        ("fol"         (init :fol         (text-content child)))
        ("pnf"         (init :pnf         (text-content child)))
        ("tptp"        (init :tptp        (text-content child)))
        ("messages"    (init :messages    (extract-messages-from-node child)))
        (otherwise (error "Unexpected element ~A." (dom:tag-name child)))))))

(defun extract-duration-from-node (node)
  ;;;; <duration tokenizer="0.000"
  ;;;;           parser="0.010"
  ;;;;           refres="0.000"/>
  (make-node-instance 'duration node
    :tokenizer '("tokenizer" :key parse-float)
    :parser    '("parser"    :key parse-float)
    :refres    '("refres"    :key parse-float)))

(defun extract-messages-from-node (node)
  (map-element-nodes 'extract-message-from-node (dom:child-nodes node)))

(defun parse-junky-integer (string)
  ;; Defining this as a standalone funtion makes it easier to use as a
  ;; key value in extract-message-from-node (since passing the symbol
  ;; parse-junky-integer is then permissible).
  (parse-integer string :junk-allowed t))

(defun message-node-message-type (node)
  ;; since messages are identified by two orthogonal components
  ;; (importance and type), it's a bit awkward to determine which kind
  ;; of message to make.  It's straightforward though.  The
  ;; appropriate kind is just ape-<type>-<importance>-message.
  (pcase (dom:get-attribute node "importance") #'string=
    ("error"
     (ecase (keywordify (dom:get-attribute node "type"))
       (:character 'ape-character-error-message)
       (:word      'ape-word-error-message)
       (:sentence  'ape-sentence-error-message)
       (:syntax    'ape-syntax-error-message)
       (:anaphor   'ape-anaphor-error-message)
       (:timelimit 'ape-timelimit-error-message)
       (:lexicon   'ape-lexicon-error-message)
       (:owl       'ape-owl-error-message)
       (:ape       'ape-ape-error-message)))
    ("warning"
     (ecase (keywordify (dom:get-attribute node "type"))
       (:character 'ape-character-warning-message)
       (:word      'ape-word-warning-message)
       (:sentence  'ape-sentence-warning-message)
       (:syntax    'ape-syntax-warning-message)
       (:anaphor   'ape-anaphor-warning-message)
       (:timelimit 'ape-timelimit-warning-message)
       (:lexicon   'ape-lexicon-warning-message)
       (:owl       'ape-owl-warning-message)
       (:ape       'ape-ape-warning-message)))))

(defun extract-message-from-node (node)
  ;;;; <message importance="error"
  ;;;;          type="sentence"
  ;;;;          sentence="1"
  ;;;;          token="3"
  ;;;;          value="Mary sells &lt;&gt; apples."
  ;;;;          repair="This ... failed."/>
  (make-node-instance (message-node-message-type node) node
    ;; importance and type slots get values by initforms in
    ;; corresponding classes.  E.g., when importance is "warning" and
    ;; type is "character", message-node-type returns
    ;; ape-character-warning-message, which has iniforms :warning and
    ;; :character for the importance and type slots, respectively.
    :sentence   '("sentence" :key parse-junky-integer)
    :token      '("token"    :key parse-junky-integer)
    :value        "value"
    :repair       "repair"))
