;;;; -*- lisp -*-

(in-package :it.bese.yaclml)

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
;;;; can compile this text into a function which, when called, prints
;;;; the corresponding HTML on *yaclml-stream*. The compiled function
;;;; requires two arguments: an environment mapping names (symbols) to
;;;; lisp objects and a generator which is used for finding other
;;;; templates to include.

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
;;;; Templates are always compiled with the default namespace bound to
;;;; the package :it.bese.yaclml.tags, this allows all the standard
;;;; HTML tags to be used without problems. The namespace identifier
;;;; http://common-lisp.net/project/bese/tal/core can be used to
;;;; specify the :it.bese.yaclml.tal package which contains all the
;;;; standard TAL tags and attributes. If it is necessary the
;;;; :it.bese.yaclml.tags namespace can be accessed via
;;;; "http://common-lisp.net/project/bese/yaclml/core". Parameters
;;;; passed to included templates need to use the
;;;; "http://common-lisp.net/project/bese/tal/params" name space.

(defvar *tal-attribute-handlers* '())

(defvar *tal-tag-handlers* '())

(defparameter *uri-to-package*
  (list 
   (cons "http://common-lisp.net/project/bese/tal/core"
         (find-package :it.bese.yaclml.tal))
   (cons "http://common-lisp.net/project/bese/tal/params"
         (find-package :it.bese.yaclml.tal.include-params))
   (cons "http://common-lisp.net/project/bese/yaclml/core"
         (find-package :it.bese.yaclml.tags))
   (cons "http://www.w3.org/XML/1998/namespace"
         (find-package :it.bese.yaclml.xml))
   (cons "http://www.w3.org/1999/xlink/"
         (find-package :it.bese.yaclml.xlink)))
  "Default mapping of xmlns to packages.")

(defvar *expression-package* nil
  "The value of *PACKAGE* when tal attribute expressions and for
  looking up symbols in the environment.")

;; TODO these are kludgy for C-c C-c, they push every time. probably a hashtable would do better...
(defmacro def-attribute-handler (attribute (tag) &body body)
  "Defines a new attribute handler name ATTRIBUTE."
  `(progn
     (push (cons ',attribute (lambda (,tag) ,@body))
           *tal-attribute-handlers*)
     ',attribute))

(defmacro def-tag-handler (tag-name (tag) &body body)
  "Defines a new tag handlec named TAG-NAME."
  `(progn
     (push (cons ',tag-name (lambda (,tag)
                              (declare (ignorable ,tag))
                              ,@body))
           *tal-tag-handlers*)
     ',tag-name))

(def-special-environment tal-compile-environment ()
  generator)

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
  ;; done iterating (or recusring) over the tags we can string-fold
  ;; ops to get the longest posible string sequences.
  (mapcar (lambda (form)
	    (transform-lxml-form form))
	  tree))

(defun transform-lxml-form (form)
  "Transforms the lxml tree FORM into common lisp code (a series
  of calls to tag macros)."
  (flet ((find-attribute-handlers (attributes)
	   (loop 
	      for (key) on attributes by #'cddr
	      for handler = (assoc key *tal-attribute-handlers* :test #'eql)
	      when handler
	      do (return-from transform-lxml-form
		   (funcall (cdr handler) form))))
	 (find-tag-handler (tag-name)
	   (dolist* ((name . handler) *tal-tag-handlers*)
             (when (eql name tag-name)
	       (return-from transform-lxml-form
		 (funcall handler form)))))
	 (handle-regular-tag (tag-name attributes body)
	   (unless (member tag-name '(:comment :xml))
	     `(,tag-name
	       ,@(loop
		    for (key value) on attributes by #'cddr
		    nconc (list (intern (symbol-name key) :keyword)
				(if (stringp value)
				    (parse-tal-attribute-value value)
				    value)))
	       ,@(transform-lxml-tree body)))))
    (if (stringp form)
	`(<:as-is ,form)
	(if (and (consp form)
		 (consp (car form)))
	    (destructuring-bind ((tag-name &rest attributes) &rest body) form
	      ;; first see if there are any attribute handlers
	      (find-attribute-handlers attributes)
	      ;; first see if there's a handler for this tag
	      (find-tag-handler tag-name)
	      ;; didn't find a handler for that tag or any of it's
	      ;; attributes, must be a "regular" yaclml tag.
	      (handle-regular-tag tag-name attributes body))
	    (error "Badly formatted YACLML: ~S." form)))))
  
(defun compile-tal-string-to-lambda (string &optional (expression-package *package*))
  "Returns the source code for the tal function form the tal text STRING."
  (bind-tal-compile-environment ((generator (gensym)))
    (with-tal-compile-environment (generator)
      `(lambda (-tal-environment- ,generator)
         (declare (ignorable -tal-environment- ,generator))
         ,(let ((*package* (find-package :it.bese.yaclml.tags))
                (*expression-package* expression-package))
            (transform-lxml-form (it.bese.yaclml.xmls:parse string :uri-to-package *uri-to-package*)))))))

(defun compile-tal-string (string &optional (expression-package (find-package :common-lisp-user)))
  (let ((*break-on-signals* t))
    (compile nil (compile-tal-string-to-lambda string expression-package))))

(defun compile-tal-file (pathname &optional (expression-package (find-package :common-lisp-user)))
  (with-tal-compilation-unit pathname
    (compile-tal-string (read-tal-file-into-string pathname) expression-package)))

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
