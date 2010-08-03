;;;; -*- lisp -*-

(in-package :talcl)

;;;; * TAL - Dynamic HTML Templating

;;;; TAL is an HTML templating mechanism designed to provide enough
;;;; power to create any possible HTML output from a template yet at
;;;; the same time maintain the distinction between presentation logic
;;;; and program logic.

;;;; TAL's syntax tries to be as HTMLish as possible, this is to help
;;;; graphic designers, who can't be bothered, and really shouldn't be
;;;; bothered, to learn a new syntax, and the most common design
;;;; tools, which work best with tags and attributes.

;;;; A TAL template is an object (represented internally as a function
;;;; and externally as a text file or a string) which, given an
;;;; environment mapping names to values and a generator which is able
;;;; to locate included templates, produces some text.

;;;; * Compiling TAL Templates

;;;; Given a TAL template (either an in-memory string or a file) We
;;;; can compile this text into a function which, when called
;;;; generates cxml sax events and should probably be done in the
;;;; context of cxml:with-xml-output. The compiled function requires
;;;; one argument: an environment mapping names (symbols) to lisp
;;;; objects. see tal-env

;;;; * The TAL Expression language

;;;; Generally we don't want lisp code to appear in TAL templates,
;;;; however there are two exceptions to this rule:

;;;; - tags or attributes which operate on values taken from the
;;;;   environment (eg: the content and dolist attributes discussed
;;;;   below) are passed regular lisp code
;;;;   (expressed as text and then converted to lisp via
;;;;   READ-FROM-STRING).

;;;; - Inside other HTML attributes where we would like to substitute
;;;;   the value of some lisp code inside the textual value of the
;;;;   attribute.

;;;; When a lisp expression is expected as the value of a TAL
;;;; attribute the '$' read macro can be used to access values in
;;;; current TAL environment.

;;;; When a string value is expected for an HTML attribute the syntax
;;;; "${...}" can be used to evaluate a lisp form and substitute the
;;;; result into the contain attribute value. The \"@{...}\" syntax
;;;; differs from the "${...}\" syntax in that the form is expected
;;;; to return a list and every element of that list is embedded in
;;;; the enclosing attribute. Inside the lisp forms the \"$\" read
;;;; macro is available as with regular lisp only attributes.

;;;; * Mapping Names to Tags and Attributes

;;;; TAL templates are xml and use xml's namespace mechanism for
;;;; defining the mapping from names to attribute and tag handlers.
;;;; The namespace identifier
;;;; http://common-lisp.net/project/bese/tal/core can be used to
;;;; specify the :net.common-lisp.project.bese.tal.core package which
;;;; contains all the standard TAL tags and attributes.  Parameters
;;;; passed to included templates need to use the
;;;; "http://common-lisp.net/project/bese/tal/params" name space.

(defvar *tal-attribute-handlers* ())

(defvar *tal-tag-handlers* ())

;;;; Interning namespaced elements to packages
(defparameter *uri-to-package*
  (list 
   (cons "http://common-lisp.net/project/bese/tal/core"
         (find-package :net.common-lisp.project.bese.tal.core))
   (cons "http://common-lisp.net/project/bese/tal/params"
         (find-package :net.common-lisp.project.bese.tal.include-params)))
  "Default mapping of xmlns to packages.")


(defclass interner (cxml:broadcast-handler)
  ((uri-to-package :initform nil :initarg :uri-to-package
		   :accessor uri-to-package)
   (ns-test :initform #'string= :initarg :ns-test :accessor ns-test)))

(defmethod sax:start-element ((handler interner)
			      namespace-uri local-name qname attributes)
  (let ((uri-to-package (uri-to-package handler))
	(test (ns-test handler)))
    (flet ((fpkg (ns) (cdr (assoc ns uri-to-package :test test))))
      
      (awhen (fpkg namespace-uri)
	(setf local-name (intern (string-upcase local-name) it)
	      namespace-uri nil))
	
      (iterate (for attr in attributes)
	       (for ans = (sax:attribute-namespace-uri attr))
	       (for pkg = (fpkg ans))
	       (when pkg
		 (let ((sym (intern (string-upcase (sax:attribute-local-name attr))
				    pkg)))
		   (setf (sax:attribute-local-name attr) sym
			 (sax:attribute-qname attr) sym
			 (sax:attribute-namespace-uri attr) nil)
		   )))))
  (call-next-method handler namespace-uri local-name qname attributes))

(defun make-interner (uri-to-package chained-handler)
  (make-instance 'interner
		 :uri-to-package uri-to-package
		 :handlers (list chained-handler)))
;;;;



(defvar *expression-package* (find-package :common-lisp-user)
  "The value of *PACKAGE* when tal attribute expressions and for
  looking up symbols in the environment.")

;; TODO these are kludgy for C-c C-c, they push every time. probably a hashtable would do better...
(defmacro def-attribute-handler (attribute (tag) &body body)
  "Defines a new attribute handler name ATTRIBUTE."
  `(progn (defun ,attribute (,tag)
	    ,@body)
	  (pushnew ',attribute *tal-attribute-handlers*)))

(defmacro def-tag-handler (tag-name (tag) &body body)
  "Defines a new tag handlec named TAG-NAME."
  `(progn (defun ,tag-name (,tag)
	    ,@body)
	  (pushnew ',tag-name *tal-tag-handlers*)))

(defun |$ tal reader| (stream char)
  "The $ char reader for tal expressions."
  (declare (ignore char))
  `(lookup-tal-variable ',(read stream) -tal-environment-))

(defun read-tal-expression-from-string (expression &optional implicit-progn-p)
  "Reads a single form from the string EXPRESSION using the TAL
 expression read table."
  (assert *expression-package*
          (*expression-package*)
          "No expression package!")
  (let ((*readtable* (copy-readtable nil))
        (*package* *expression-package*))
    ;; use $SYMBOL to access the value of the environment variable
    ;; SYMBOL
    (set-macro-character #\$ #'|$ tal reader| nil *readtable*)
    (if implicit-progn-p
        (iter (with pos = 0)
              (for (values obj new-pos) = (read-from-string expression nil nil :start pos))
              (while obj)
              (setf pos new-pos)
              (collect obj into result)
              (finally (return (if (> (length result) 1)
                                   (cons 'progn result)
                                   (first result)))))
        (read-from-string expression))))

(defun parse-tal-attribute-value (value-string)
  "Parser a TAL attribute expression, returns a form for building
  the expression at run time."
  (let ((parts '()))
    (with-input-from-string (val value-string)
      (with-output-to-string (text)
        (flet ((read-tal-form ()
                 (let ((*readtable* (copy-readtable nil))
                       (*package* *expression-package*))
                   (set-macro-character #\} (get-macro-character #\)) nil *readtable*)
                   (set-macro-character #\$ #'|$ tal reader| nil *readtable*)
                   (read-delimited-list #\} val))))
          (loop
             for char = (read-char val nil nil)
             while char
             do (case char
                  (#\\ (let ((next-char (read-char val nil nil)))
                         (if (null next-char)
                             (error "Parse error in ~S. #\\ at end of string." value-string)
                             (write-char next-char text))))
                  (#\$ (let ((next-char (peek-char nil val nil nil nil)))
                         (if (and next-char (char= #\{ next-char))
                             (progn
                               (read-char val nil nil nil)
                               ;; first push the text uptil now onto parts
                               (let ((up-to-now (get-output-stream-string text)))
                                 (unless (string= "" up-to-now)
                                   (push up-to-now parts)))
                               ;; now push the form
                               (push `(princ-to-string (progn ,@(read-tal-form))) 
                                     parts))
                             (write-char #\$ text))))
                  (#\@ (let ((next-char (peek-char nil val nil nil)))
                         (if (and next-char (char= #\{ next-char))
                             (progn
                               (read-char val nil nil nil)
                               ;; first push the text uptil now onto parts
                               (push (get-output-stream-string text) parts)
                               ;; now push the form
                               (let ((form (read-tal-form))
                                     (stream (gensym))
                                     (i (gensym)))
                                 (push `(with-output-to-string (,stream)
                                          (dolist (,i (progn ,@form))
                                            (princ ,i ,stream)))
                                       parts)))
                             (write-char #\@ text))))
                  (t
                   (write-char char text)))
               finally (let ((remaining-text (get-output-stream-string text)))
                         (unless (string= "" remaining-text)
                           (push remaining-text parts)))))))
    ;; done parsing, parts now contains everything to put in the
    ;; list, but in reverse order.
    (case (length parts)
      (0 "")
      (1 (car parts))
      (t `(concatenate 'string ,@(nreverse parts))))))

(defun transform-lxml-tree (tree)
  "Given a tree representing some LXML code with TAL attributes
  returns the yaclml tag using code for generating the
  HTML. Destructivly modifies TREE."
  ;; we collect strings and forms it the collector OPS. When we're
  ;; done iterating (or recursing) over the tags we can string-fold
  ;; ops to get the longest posible string sequences.
  (mapcar (lambda (form)
	    (transform-lxml-form form))
	  tree))

(defun transform-lxml-form (form)
  "Transforms the lxml tree FORM into common lisp code (a series
  of calls to tag macros)."
  (labels
      ((find-handler-named (name)
	 (awhen (and (symbolp name)
		     (fboundp name) )
	   (return-from transform-lxml-form
	     (funcall (symbol-function name) form))))

       (find-attribute-handlers (attributes)
	 (dolist (attr attributes)
	   (find-handler-named (first attr))))

       (handle-regular-tag (tag-name attributes body)
	 (unless (member tag-name '(:comment :xml))
	   (let* ((namespaces nil)
		  (buildnode:*namespace-prefix-map*
		   buildnode:*namespace-prefix-map*)
		  (attrib-forms
		   (flet ((add-ns (prefix ns)
			    (push (list prefix ns) namespaces)
			    (push (cons ns prefix)
				  buildnode:*namespace-prefix-map*)
			    nil))
		     (loop
		       for (key pre-value) in attributes
		       for value = (if (stringp pre-value)
				       (parse-tal-attribute-value pre-value)
				       pre-value)
		       append (if (consp key)
				  (destructuring-bind (lname . ns) key
				    ;; is it an xmlns attrb?
				    (if (string= ns
						 "http://www.w3.org/2000/xmlns/")
					(add-ns lname value)
					`((cxml:attribute*
					   ,(buildnode::get-prefix ns)
					   ,lname
					   ,value))))
				  `((cxml:attribute* nil ,key ,value))))))
		  (element-form `(cxml:with-element*
				     (,(and (consp tag-name)
					    (buildnode::get-prefix
					     (cdr tag-name)))
				       ,(if (consp tag-name)
					    (car tag-name)
					    tag-name))
				   ,@attrib-forms
				   ,@(transform-lxml-tree body))))
	     (dolist (p namespaces)
	       (setf element-form `(cxml:with-namespace ,p
				     ,element-form)))
	     element-form))))
    
    (if (stringp form)
	`(cxml:text ,form)
	(if (consp form)
	    (destructuring-bind (name attributes &rest body) form
	      (let ((tag-name
		     (if (consp name)	; (name . namespace)
			 (let ((pkg (cdr (assoc (cdr name) *uri-to-package*
						:test #'string=))))
			   (if pkg
			       (intern (string-upcase (car name)) pkg)
			       name))
			 name)))
		
		;; first see if there are any attribute handlers
		(find-attribute-handlers attributes)
		;; first see if there's a handler for this tag
		(find-handler-named tag-name)
		;; didn't find a handler for that tag or any of it's
		;; attributes, must be a "regular" yaclml tag.
		(handle-regular-tag tag-name attributes body)))
	    (error "Badly formatted YACLML: ~S." form)))))
  
(defun compile-tal-string-to-lambda (string &optional (expression-package *package*))
  "Returns the source code for the tal function form the tal text STRING."
  `(lambda (-tal-environment-)
     (declare (ignorable -tal-environment-)
	      (optimize (debug 3)))
     ,(let ((*package* (find-package :ucw))
	    (*expression-package* expression-package)
	    (parse-tree (cxml:parse string
				    (make-interner *uri-to-package*
						   (cxml-xmls:make-xmls-builder
						    :include-namespace-uri t)))))
	(transform-lxml-form parse-tree))))

(defun compile-tal-string (string &optional
			   (expression-package (find-package :common-lisp-user)))
  (let* ((*break-on-signals* t)
	 (lamb (compile-tal-string-to-lambda string expression-package)))
    (compile nil lamb)))

(defun compile-tal-file (pathname &optional
			 (expression-package (find-package :common-lisp-user)))
  (restart-case
      (with-tal-compilation-unit pathname
	(compile-tal-string (read-tal-file-into-string pathname) expression-package))
    (retry-compile-tal-file ()
      :report (lambda (stream)
		(format stream "Retry compiling template ~s" pathname))
      (compile-tal-file pathname expression-package))))

(define-condition tal-compilation-condition (simple-condition)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report (lambda (c s)
	     (apply #'format
	      s
	      (format-control c)
	      (format-args c)))))

(define-condition tal-compilation-warning (tal-compilation-condition warning) ())

(defun tal-warn (message &rest args)
  (warn (make-condition 'tal-compilation-warning
			:format-control message
			:format-args args)))

(define-condition tal-compilation-error (tal-compilation-condition error) ())
(defun tal-error (message &rest args)
  (error (make-condition 'tal-compilation-error
			 :format-control message
			 :format-args args)))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
