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
(defvar *string-being-compiled*)
(defvar *name-being-compiled* ())

(defvar *talable-strings* T
  "Whether or not to treat regular attributes and contents as talable strings.")

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

;;;; HANDLER helpers

(defun pull-attrib-val! (tag key)
  (when-bind attr (find key (second tag) :key #'car)
    (setf (second tag) (remove attr (second tag)))
    (second attr)))

(defmacro destructure-tag ((tag &rest vars) &body body)
  "Binds tag-name tag-attributes and tag-body"
  (rebinding (tag)
    (let ((vars (iter (for var in vars)
		      (collect
			  `(,var (pull-attrib-val! ,tag ',var))))))
      `(let ,vars
	 (destructuring-bind (tag-name tag-attributes &rest tag-body) ,tag
	   (declare (ignorable tag-name tag-attributes tag-body))
	   ,@body)))))

;;;;;;;;;;;;;;

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

(defun find-handler-named (name)
  (when (and (symbolp name)
             (fboundp name))
    (symbol-function name)))

(defun |$ tal reader| (stream char)
  "The $ char reader for tal expressions.
   This just tries to smooth over inconsistancies encountered by using
   $var-name in tal:foo attributes and regular foo attributes"
  (declare (ignore char))
  (read stream))

(defun read-tal-expression-from-string (expression &optional implicit-progn-p)
  "Reads a single form from the string EXPRESSION using the TAL
 expression read table."
  (assert *expression-package*
          (*expression-package*)
          "No expression package!")
  (let ((*readtable* (copy-readtable nil))
        (*package* *expression-package*))
    ;; SYMBOL
    (set-macro-character #\$ #'|$ tal reader| nil *readtable*)
    (if implicit-progn-p
        (iter (with pos = 0)
              (for (values obj new-pos) = (read-from-string expression nil nil :start pos))
              (while obj)
              (setf pos new-pos)
              (collect obj into result)
              (finally
	       (return (if (> (length result) 1)
			   (cons 'progn result)
			   (first result)))))
        (read-from-string expression))))

(defun parse-talable-string (value-string)
  "Parser a TAL attribute expression, returns a form for building
  the expression at run time."
  (unless *talable-strings*
    (return-from parse-talable-string (list value-string)))
  (let ((parts '()))
    (with-input-from-string (val value-string)
      (with-output-to-string (text)
        (flet ((read-tal-bracketed-form ()
		 (let ((*readtable* (copy-readtable nil))
                       (*package* *expression-package*))
		   (set-macro-character #\} (get-macro-character #\)) nil *readtable*)
		   (set-macro-character #\$ #'|$ tal reader| nil *readtable*)
		   (read-delimited-list #\} val)))
	       (read-tal-form ()
                 (let ((*readtable* (copy-readtable nil))
                       (*package* *expression-package*))
		   (set-macro-character #\$ #'|$ tal reader| nil *readtable*)
		   (read-preserving-whitespace val))))
          (loop
             for char = (read-char val nil nil)
             while char
             do (case char
                  (#\\ (let ((next-char (read-char val nil nil)))
                         (if (null next-char)
                             (error "Parse error in ~S. #\\ at end of string." value-string)
                             (write-char next-char text))))
                  (#\$ (let ((next-char (peek-char nil val nil nil nil)))
			 (if (not next-char)
			     (error-tal-expression-eof value-string)
			     (handler-case 
				 (cond
				   ;; escaped $
				   ((char= #\$ next-char)
				    (read-char val nil nil nil)
				    (write-char #\$ text))

				   (t ;; tal-expression
				    (let ((bracketed? (char= #\{ next-char)))
				      ;; remove open { if nec
				      (when bracketed?
					(read-char val nil nil nil))
				      ;; first push the text uptil now onto parts
				      (let ((up-to-now (get-output-stream-string text)))
					(unless (string= "" up-to-now)
					  (push up-to-now parts)))
				      ;; now push the form
				      (push (if bracketed?
						`(progn ,@(read-tal-bracketed-form))
						(read-tal-form))
					    parts))
				    ))
			       (END-OF-FILE (e)
				 (error-tal-expression-eof value-string e))))))

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
    (nreverse parts)))

(defun parse-tal-attribute-value (value-string)
  (flet ((form-for-part (p)
	   (typecase p
	     (string p)
	     (T `(aif ,p (princ-to-string it) "")))))
    (let ((parts (parse-talable-string value-string)))
      (case (length parts)
	(0 "")
	(1 (form-for-part (first parts)))
	(t `(concatenate 'string
			 ,@(iter (for p in parts)
				 (collect (form-for-part p)))))))))

(defun parse-tal-body-content (value-string)
  (let ((parts (parse-talable-string value-string)))
    (case (length parts)
      (0 nil)
      (1 `(%emit-tagged-content ,(car parts) T))
      (t `(progn
            ,@(iter (for p in parts)
                   (collect `(%emit-tagged-content ,p T))))))))

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
  (typecase form
    (string (parse-tal-body-content form))
    (cons (destructuring-bind (tag-name attributes &rest body) form
            ;; the cxml parser reverses attribute order, which
            ;; is problematic.  Reverse these so that tal produces
            ;; the expected output
            (let ((attributes (reverse attributes)))
              ;; first see if there are any attribute handlers
              (dolist (attr attributes)
                (awhen (find-handler-named (first attr))
                  (return-from transform-lxml-form
                    (funcall it form))))

              ;; first see if there's a handler for this tag
              (aif (find-handler-named tag-name)
                   (funcall it form)
                   ;; didn't find a handler for that tag or any of it's
                   ;; attributes, must be a "regular" tag.
                   (transform-lxml-regular-tag tag-name attributes body)))))
    (t (error "Badly formatted TAL: ~S." form))))


(defun specified-attribute* (prefix lname value &optional qname)
  "A copy of cxml:attribute* that sets specified-p correctly
   TODO: cxml:attribute* should probably set this by default or
         accept it as an optional arg.  Push this upstream

   @arg[prefix]{Namespace prefix, a string.}
   @arg[lname]{Local name, a string.}
   @arg[value]{Any value understood by @fun{unparse-attribute}, in particular
     strings.}
   @return{undocumented}

   Collects an attribute for the start tag that is currently being written.

   This function may only be called during the extent of a use of
   @fun{with-element} or @fun{with-element*}, and only before the first
   child node has been written.
   
   An attribute for the current element is recorded using the namespace prefix
   and local name specified by arguments.  @var{prefix} is resolved to a
   namespace URI using the bindings established by @fun{with-namespace},
   and that namespace URI is used for the attribute."
  (setf value (cxml:unparse-attribute value))
  (when value
    (setf prefix (when prefix (runes:rod prefix)))
    (setf lname (runes:rod lname))
    (push (sax:make-attribute
	   :namespace-uri (cxml::find-unparse-namespace prefix)
	   :local-name lname
	   :qname (or qname
		      (if prefix (concatenate 'runes:rod prefix #":" lname) lname))
	   :value (runes:rod value)
	   ;; THIS SHOULD BE SPECIFIED  TODO:Push upstream
	   :specified-p T)
	  (cdr cxml::*current-element*))))

(defun transform-lxml-regular-tag (tag-name attributes body)
  (case tag-name
    (:comment `(cxml:comment ,(first body)))
    (:xml)
    (T (let* ((namespaces nil)
              (buildnode:*namespace-prefix-map* buildnode:*namespace-prefix-map*)
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
                   collect (if (consp key)
                               (destructuring-bind (lname . ns) key
                                 ;; is it an xmlns attrb?
                                 (if (string= ns "http://www.w3.org/2000/xmlns/")
                                     (add-ns lname value)
                                     `(cxml:attribute*
                                       ,(buildnode::get-prefix ns)
                                       ,lname
                                       ,value)))
                               `(specified-attribute* nil ,key ,value)))))
              ;;the attrib-forms must be done ahead of this so
              ;;that get-prefix can lookup in the ammended
              ;;namespace-prefix-map
              (tag-name-spec (if (consp tag-name)
                                 (destructuring-bind (lname . ns) tag-name
                                   `(,(buildnode::get-prefix ns) ,lname))
                                 `(nil ,tag-name)))
              (element-form `(cxml:with-element* ,tag-name-spec
                               ,@attrib-forms
                               ,@(transform-lxml-tree body))))
         (dolist (p namespaces)
           (setf element-form `(cxml:with-namespace ,p
                                 ,element-form)))
         element-form))
    ))

;; this is used to pass variables up to the enclosing scope
;; from lower in the tal tree, (ie def tags)
(defvar *tal-defs*)

(defun transform-lxml-form-in-scope ( form )
  "Creates a new scope (ie: for def) and processes a form inside it"
  (let* (*tal-defs*
	 (processed-body (transform-lxml-form form)))
    (if *tal-defs*
	`(let (,@*tal-defs*) ,processed-body)
	processed-body)  
    ))

(defun transform-lxml-tree-in-scope ( tree )
  "Creates a new scope (ie: for def) and processes a tree of children inside it"
  (let* (*tal-defs*
	 (processed-body (transform-lxml-tree tree)))
    (if *tal-defs*
	`(let (,@*tal-defs*) ,processed-body)
	processed-body)))

(defun call-lambda-with-unbound-variable-restarts (lambda)
  "When you enconter an unbound variable while executing a template function,
    provide a restart to bind that variable so that the template can be executed

   see also: call-lambda-with-default-missing-value
      which will invoke this restart with *default-missing-template-value*
      if there is one

   see also: with-missing-value-handler which shortcuts the rather tedious
      handler-bind for this into a bare minimum handler
  "
  (let ( unbound-vars unbound-vals missing )
    (labels ((read-new-value ()
	       (format t "Enter a new value: ")
	       (multiple-value-list (eval (read))))
	     (try-run ()
	       (restart-case 
		   (progv unbound-vars unbound-vals 
		     (handler-bind
			 ;; If we encounter an unbound variable
			 ;; bind missing (for use in the restart) and allow the error
			 ;; to bubble
			 ((unbound-variable
			   (lambda (c) (setf missing (cell-error-name c))))
			  (tal-runtime-condition
			   (lambda (c)
			     (when (typep (original-error c) 'unbound-variable)
			       (setf missing (cell-error-name (original-error c)))))))
		       (funcall lambda)))
		 (set-value (val)
		   ;; after setting the unbound variable rerun the template function
		   :report (lambda (s) (format s "Set A Value for ~A" missing))
		   :interactive read-new-value
		   (push missing unbound-vars)
		   (push val unbound-vals)
		   (try-run)))))
      (try-run))))

(defvar *default-missing-template-value*)
;; A default value to splice in whenever you encounter an unbound variable

(defmacro with-missing-value-handler (((&optional name) &body handler-body) &body body)
  "Provides error handler that will handle filling unbound variable
   references with a value

   handler can be (() ... body ) or ((name) ... body)
    the return value of handler will be the value inserted in place of the missing value
    If you provide name, it will be bound to the name of the unbound variable for the
    duration of the handler

   see call-lambda-with-default-missing-value for an example of use "
  (with-unique-names (handler uvar)
    (let ((var (or name uvar)))
      `(flet ((,handler (,var) (declare (ignorable ,var))
		,@handler-body))
	 (handler-bind
	     ((unbound-variable
	       (lambda (c)
		 (when (find-restart 'talcl:set-value)
		   (invoke-restart 'talcl:set-value
				   (funcall #',handler (cell-error-name c))))))
	      (talcl:tal-runtime-condition
	       (lambda (c)
		 (when (and (typep (original-error c) 'unbound-variable)
			    (find-restart 'talcl:set-value))
		   (invoke-restart 'talcl:set-value
				   (funcall #',handler (cell-error-name
							(original-error c))))))))
	   ,@body)))))

(defun call-lambda-with-default-missing-value (lambda)
  "If you encounter an unbound template variable and we have a
   *default-missing-template-value*, invoke the set-value restart with
   that default

   see also: call-lambda-with-default-missing-value which sets up the restart
  "  
  (if (boundp '*default-missing-template-value*)
      (with-missing-value-handler (() *default-missing-template-value*)
	(call-lambda-with-unbound-variable-restarts lambda))
      (call-lambda-with-unbound-variable-restarts lambda)))

(defun compile-tal-parse-tree-to-lambda (parse-tree
					 &optional (expression-package *package*)
					 tree-or-forms? )
  (let* ((*package* (find-package :talcl))
	 (*expression-package* expression-package))
    `(lambda (&optional -tal-environment-) ;; Just used for debugging at this point
       (declare (ignorable -tal-environment-))
       ;; lexically bind the name being compiled so it can be included in error messages
       (let ((name-being-compiled ,*name-being-compiled*)) 
	 (flet ((body ()
		  (handler-bind
		      ;; Put more information on errors thrown from compiled tal functions
		      ;; so that when deeply nested templates throw an error
		      ;; you can track down which template it originated in
		      ((error #'(lambda (e)
				  (tal-runtime-error
				   e
				   "Compiled Tal ~s~%threw an error: ~a "
				   name-being-compiled e))))
		    ,(if tree-or-forms?
			 `(progn ,@(transform-lxml-tree-in-scope parse-tree) )
			 (transform-lxml-form-in-scope parse-tree)))))
	   (call-lambda-with-default-missing-value #'body)
	   )))))

(defun compile-tal-string-to-lambda (string &optional (expression-package *package*))
  "Returns the source code for the tal function form the tal text STRING."
  (let* ((*string-being-compiled* string)
	 (*package* (find-package :talcl))
	 (*expression-package* expression-package)
	 (parse-tree (cxml:parse string
				 (make-interner *uri-to-package*
						(make-extended-xmls-builder
						 :include-namespace-uri t)))))
    (compile-tal-parse-tree-to-lambda parse-tree expression-package)))

(defun compile-tal-string (string &optional
			   (expression-package (find-package :common-lisp-user)))
  (let* ((*break-on-signals* nil)
	 (lamb (compile-tal-string-to-lambda string expression-package)))
    ;(break "~S" lamb)
    (compile nil lamb)))

(defun compile-tal-file (pathname &optional
			 (expression-package (find-package :common-lisp-user)))
  (let ((*name-being-compiled* pathname))
  (restart-case
      (with-tal-compilation-unit pathname
	(compile-tal-string (read-tal-file-into-string pathname) expression-package))
    (retry-compile-tal-file ()
      :report (lambda (stream)
		(format stream "Retry compiling template ~s" pathname))
      (compile-tal-file pathname expression-package)))))

(define-condition tal-compilation-condition (simple-condition)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report (lambda (c s)
	     (apply #'format
	      s
	      (format-control c)
	      (format-args c)))))

(define-condition tal-runtime-condition (simple-condition)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil)
   (original-error :accessor original-error :initarg :original-error :initform nil))
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

(defun find-expression-line/col-number (expr)
  (with-input-from-string (s *string-being-compiled*)
    (let ((target (search expr *string-being-compiled* :test #'char=)))
      (when target
	(iter (for line in-stream s using #'dos-safe-read-line )
	      (for line-len = (+ 1 (length line))) ;;dont forget newlines
	      (for start-len = total-len)
	      (summing line-len into total-len)
	      (counting line into lines)
	      (when (> total-len target)
		(return (format nil "line: ~s col: ~s, line: ~s" lines (- target start-len) line)))
	      )))))

(define-condition tal-unexpected-eof-compilation-error (tal-compilation-error error) ())
(defun error-tal-expression-eof (expression &optional error)
  (let* ((loc (find-expression-line/col-number expression))
	 (message (with-output-to-string (s)
		    (princ "Unexpected EOF in tal expression starting with $~% Input: ~S" s)
		    (when loc
		      (princ "~% Estimated Location: ~a" s))
		    (when error
		      (princ "~% Error: ~s" s)))))
    (error (make-condition 'tal-unexpected-eof-compilation-error
			   :format-control message
			   :format-args (remove nil (list expression loc error))))))

(defun tal-runtime-error (original-error message &rest args)
  (error (make-condition 'tal-runtime-condition
			 :original-error original-error
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
