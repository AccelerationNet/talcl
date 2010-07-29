(in-package :ucw)
(defun test-parse ()
  (let* ((it.bese.yaclml.xmls::*strip-comments* nil)
	 (str (arnesi:read-string-from-file "/home/ACCELERATION/nathan/lisp/gainesville-green/templates/faq.tal"))
	 (form2 (cxml:parse str
			    (make-interner *uri-to-package*
					   (cxml:make-whitespace-normalizer
					    (cxml-xmls:make-xmls-builder
					     :include-namespace-uri t))))))
    form2))

(defun test-tran ()
  (let* ((form (test-parse))
	 (*expression-package* *package*)
	 (translated (transform-lxml-form form))
	 (lam
	  (bind-tal-compile-environment ((generator (gensym)))
	    (with-tal-compile-environment (generator)
	      `(lambda (-tal-environment- ,generator)
	 
		 (declare (optimize (debug 3)))
		 (cxml:with-xml-output
		     (cxml:make-character-stream-sink *standard-output*
						      :indentation 2
						      :canonical nil)
		   ,translated)
		 nil)))))
    lam))

(Defun test-eval ()
  (let ((lam (test-tran)))
    (funcall (eval lam) nil)))


(defun test-tran2 ()
  (let ((pathname "/home/ACCELERATION/nathan/lisp/gainesville-green/templates/faq.tal"))
    (with-tal-compilation-unit pathname
      (compile-tal-string-to-lambda
       (read-tal-file-into-string pathname)))))