(defun test-parse ()
  (let* ((it.bese.yaclml.xmls::*strip-comments* nil)
	 (str (arnesi:read-string-from-file "/home/ACCELERATION/nathan/lisp/gainesville-green/templates/home-search.tal"))
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
	 (lam `(lambda (-tal-environment-)
		 (declare (optimize (debug 3)))
		 (cxml:with-xml-output
		     (cxml:make-character-stream-sink *standard-output*
						      :indentation 2
						      :canonical nil)
		   ,translated)
		 nil)))
    lam))

(defun test-eval ()
  (let ((lam (test-tran)))
    (funcall (eval lam) nil)))

