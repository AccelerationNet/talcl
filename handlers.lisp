;; -*- lisp -*-

(in-package :ucw)

;;;; * Standard TAL handlers

(defun pull-attrib-val! (tag key)
  (when-bind attr (find key (second tag) :key #'car)
    (setf (second tag) (remove attr (second tag)))
    (second attr)))


(def-tag-handler tal::tal (tag)
  `(progn ,@(transform-lxml-tree (cddr tag))))

(def-attribute-handler tal::content (tag)
  "Becomes a TAL:REPLACE."
  (let ((value (pull-attrib-val! tag 'tal::content))
        (escape-html (pull-attrib-val! tag 'tal::escape-html)))
    (destructuring-bind (tag-name attributes &rest body) tag
      (declare (ignore body))
      (transform-lxml-form
       `(,tag-name ,attributes
		   (CONTENT-DUMMY ((tal::replace ,value)
				   (tal::escape-html ,escape-html))
				  "DUMMY"))))))


(def-attribute-handler tal::content-as-is (tag)
  "Becomes a TAL:REPLACE."
  (let ((value (pull-attrib-val! tag 'tal::content-as-is)))
    (destructuring-bind (tag-name attributes &rest body) tag
      (declare (ignore body))
      (transform-lxml-form
       `(,tag-name ,attributes
		   (CONTENT-DUMMY ((tal::replace ,value)
				   (tal::escape-html "nil"))
				  "DUMMY"))))))

(def-attribute-handler tal::replace (tag)
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::replace))))
    (when value
      (let ((escape (if-bind escape (pull-attrib-val! tag 'tal::escape-html)
		      ;; if they supplied a vlaue then use it (either nil
		      ;; or t or whatever)
		      (read-tal-expression-from-string escape)
		      ;; no value supplied, default to T
		      t)))
	(rebinding (value)
	  `(etypecase ,value
	     (list (eval ,value))
	     (string ,(if escape
			  `(cxml:text ,value)
			  `(cxml:unescaped ,value)))
	     ))))))

(def-attribute-handler tal::when (tag)
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::when))))
    `(when ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::unless (tag)
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::unless))))
    `(unless ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::dolist (tag)
  "On each iteration the environment is extended with the value in
  the value passed to DOLIST."
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::dolist))))
    (with-unique-names (loop-item-sym)
      `(dolist (,loop-item-sym ,value)
         (let ((-tal-environment- (extend-environment ,loop-item-sym -tal-environment-)))
           ,(transform-lxml-form tag))))))

(def-attribute-handler tal::let (tag)
  "Extend environment with a given list of bindings, as for LET form."
  (let ((bindings
         (loop
            for (name value) in (read-tal-expression-from-string
				 (pull-attrib-val! tag 'tal::let))
            collect `(cons ',name ,value))))
    `(let ((-tal-environment- (extend-environment (list (list ,@bindings)) -tal-environment-)))
       ,(transform-lxml-form tag))))

(def-tag-handler tal::lisp (tag)
  (read-tal-expression-from-string (first (cdr tag))))

(def-tag-handler tal::loop (tag)
  "On each iteration the environment is extended with the value in
  the value passed to DOLIST.

<div class='map-control check-list efficiency' >
	<span class='text'>By Efficiency</span>
        <tal:loop tal:var='class' tal:list='$efficiencies'>
	  <label class='map-control-item'>
	    <img class='marker-icon $class' />
	    <span class='text'>$class</span>
	    <input type='checkbox' name='filter-efficiency' item-type='$class' />
	  </label>
        </tal:loop>
      </div>
"
  (let ((name (intern (string-upcase
		       (pull-attrib-val! tag 'tal::var))
		      *expression-package*))
	(value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::list))))
    (destructuring-bind (tag-name attributes &rest body) tag
      (declare (ignore tag-name attributes))
      (with-unique-names (loop-item-sym)
	`(loop for ,loop-item-sym in ,value
	       append
	    (let ((-tal-environment-
		   (extend-environment (tal-env ',name ,loop-item-sym)
				       -tal-environment-)))
	      (list ,@(transform-lxml-tree body))))))))

(defpackage :it.bese.yaclml.tal.include-params
  (:use))

(def-tag-handler tal:include (tag)
  ;; <tal:include param:foo="foo" bar="$bar">
  ;;   <param:contents>
  ;;     <div tal:when="$flag">asdfjkl</div>
  ;;   </param:contents>
  ;; </tal>
  (let ((template-name
	 (or
	  (pull-attrib-val! tag 'tal::name)
	  (awhen (pull-attrib-val! tag 'tal::name-expression)
	    (parse-tal-attribute-value it))
	  (error "Missing TAL:NAME and TAL:NAME-EXPRESSION tags."))))

    (destructuring-bind (name attributes &rest body) tag
      (declare (ignore name))
      (with-collector (augmented-env)
	;; 1) grab all the attribute params
	(loop
	  for (param value) in attributes
	  if (eql (find-package :it.bese.yaclml.tal.include-params)
		  (symbol-package param))
	    do (augmented-env `(quote ,(intern (string param) *expression-package*))
			       (read-tal-expression-from-string value))
	  else
	    do (warn "Ignoring attribute in TAL:INCLUDE: ~S (~S)."
		     param (symbol-package param)))
	
	;; 2) grab all the body params
	;; 
	(dolist (child body)
	  (unless (stringp child)
	    (destructuring-bind (param-name attributes &rest body) child
	      (declare (ignore attributes))
	      (if (eql (find-package :it.bese.yaclml.tal.include-params)
		       (symbol-package param-name))
		  (augmented-env `(quote ,(intern (string param-name) *expression-package*))
				 ;; this used to be evaluated into a string 
				 ;;`(with-output-to-string (*yaclml-stream*)
				 ;;   ,@(mapcar #'transform-lxml-form body))

				 ;;but now we're just returning the
				 ;; cxml code snippet, which may
				 ;; result in some weird enviornment
				 ;; collisions.
				 `(quote (progn ,@(mapcar #'transform-lxml-form body)))
				 )
		  (warn "Ignoring body tag in TAL:INCLUDE: ~S." param-name)))))
	;; 3) GO!
	;; TODO: Figure out the generator logic and make sure this still works.
	(with-tal-compile-environment (generator)
	  `(funcall (load-tal ,generator
			      ,(if (constantp template-name)
				   (merge-pathnames template-name *tal-truename*)
				   `(let ((tal-truename ,*tal-truename*))
				      (merge-pathnames ,template-name tal-truename))))
		    (extend-environment (tal-env ,@(augmented-env)) -tal-environment-)
		    ,generator))))))


(def-attribute-handler tal::in-package (tag)
  (let ((pkg-string (pull-attrib-val! tag 'tal::in-package)))
    (let ((*expression-package*
	   (or (find-package (read-from-string pkg-string))
	       (error "No package named ~S found." (read-from-string pkg-string)))))
      (transform-lxml-form tag))))

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

