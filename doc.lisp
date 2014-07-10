;;;; This file isn't part of the source of CL-ACE, but the commands
;;;; used for generating the documentation. When the final form is
;;;; evaluted, the return value is (quote editor:setup-indent). The
;;;; setup-indent is because there as LispWorks-only forms in the
;;;; source (for editor indenting). As to the quote, I'm not sure.

(asdf:oos 'asdf:load-op '#:cldoc)

(cldoc:define-descriptor-handler
    ;; Why does writing "define-drs-condition" here work while writing
    ;; "ace-drs:define-drs-condition" does not?
    define-drs-condition (form) "drs-condition"
  (setf (car form) 'ace-drs::define-drs-condition)
  (values nil :restart (list (macroexpand form))))

(cldoc:define-descriptor-handler
    ;; Why doesn't this one work?
    define-ape-error (form) "ape-error"
    (setf (car form) 'ape-webservice::define-ape-error)
    (values nil :restart (list (macroexpand form))))
  
(cldoc:extract-documentation
 'cludg:html
 (namestring
  (make-pathname
   :directory (append (pathname-directory *load-truename*) '("doc"))))
 (asdf:find-system '#:cl-ace)
 :filter 'cludg::default-filter
 :table-of-contents-title "CL-ACE --- Lisp Bindings for Attempto Tools"
 :copy-css-into-output-dir t
 :section-names (list* "Syntax:"
                       "Examples:"
                       "Description:"
                       cludg:+default-section-names+))
