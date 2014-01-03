;; -*- lisp -*-

(in-package :talcl)

;;;; * Standard TAL handlers

(defun intern-tal-string (s)
  (intern (string-upcase s) *expression-package*))

(def-tag-handler tal::tal (tag)
  `(progn ,@(transform-lxml-tree (cddr tag))))

(def-tag-handler tal::without-reader (tag)
  (let ((*talable-strings* nil))
    `(progn ,@(transform-lxml-tree (cddr tag)))))

(def-attribute-handler tal::talable-strings (tag)
  (destructure-tag (tag tal::talable-strings)
    (let ((*talable-strings* (read-tal-expression-from-string tal::talable-strings)))
      (transform-lxml-form
       `(,tag-name ,tag-attributes ,@tag-body)))))


(def-tag-handler %eval (tag)
  (third tag))

(def-attribute-handler tal::plist-attrs (tag)
  "Convert a plist returned from the value of this attribute into attributes on this tag."
  (flet ((emit-attributes (plist)
           (iter (for (key value) on plist by #'cddr)
             (for lname = (if (consp key) (car key) key))
             (for ns = (when (consp key) (cdr key)))
             (if (string= ns "http://www.w3.org/2000/xmlns/")
                 (error "Can't handle xmlns here.")
                 (specified-attribute*
                  (and ns (buildnode::get-prefix ns))
                  lname value)))))
  
    (destructure-tag (tag tal::plist-attrs)
      (transform-lxml-form
       `(,tag-name ,tag-attributes
         (%eval ()
          (funcall ,#'emit-attributes
           ,(read-tal-expression-from-string tal::plist-attrs)))
         ,@tag-body))
      )))


(def-attribute-handler tal::content (tag)
  "ATTRIBUTE-HANDLER:
Replaces the content of the tag with the evaluated value of the
attribute. This tag escapes its content to be html-safe.

Example:
<div tal:content='$foo' class='jack'/>
Gets output as
<div class='jack'>
  |Whatever was in $foo|
</div>
"
  ;;becomes a tal:replace
  (destructure-tag (tag tal::content tal::escape-html)
      (transform-lxml-form
       `(,tag-name ,tag-attributes
		   (CONTENT-DUMMY ((tal::replace ,tal::content)
				   (tal::escape-html ,tal::escape-html))
				  "DUMMY")))))


(def-attribute-handler tal::content-as-is (tag)
  "ATTRIBUTE-HANDLER:
Replaces the content of the tag with the evaluated value of the
attribute. This tag does not escape the content.

Example:
<div tal:content-as-is='$foo' class='jack'/>
Gets output as
<div class='jack'>
  |Whatever was in $foo|
</div>
"
  ;;becomes a tal:replace
  (destructure-tag (tag tal::content-as-is)
    (transform-lxml-form
     `(,tag-name ,tag-attributes
		 (CONTENT-DUMMY ((tal::replace ,tal::content-as-is)
				 (tal::escape-html "nil"))
				"DUMMY")))))

(def-attribute-handler tal::replace (tag)
  "ATTRIBUTE-HANDLER: Replaces the content of the tag with the
evaluated value of the attribute. Whether the content is escaped
depends upon the sibling attribute 'tal:escape-html

Example:
<div tal:replace='$foo' class='jack'/>

Gets output as:
  |Whatever was in $foo|
"
  (let* ((v (pull-attrib-val! tag 'tal::replace))
	 (value (typecase v
		  (list v)
		  (string (read-tal-expression-from-string v)))))
    (when value
      (let ((escape (if-bind escape (pull-attrib-val! tag 'tal::escape-html)
		      ;; if they supplied a vlaue then use it (either nil
		      ;; or t or whatever)
		      (read-tal-expression-from-string escape)
		      ;; no value supplied, default to T
		      t)))
	`(%emit-tagged-content ,value ,escape)))))


(define-compiler-macro %emit-tagged-content (&whole form value &optional escape)
  (typecase value
    (null nil)
    (string
       (if (constantp escape)
           (if escape
               `(cxml:text ,value)
               `(cxml:unescaped ,value))
           `(if ,escape
                (cxml:text ,value)
                (cxml:unescaped ,value))))
    (T form)))

(defgeneric %emit-tagged-content (value &optional escape)
  (:method (value &optional escape)
    (typecase value
      (null nil)
      (string (if escape
                  (cxml:text value)
                  (cxml:unescaped value)))
      (dom:node (dom-walk-helper value))
      (function (funcall value escape))
      (buffering-sink
       ;; makes sure our sink is ready by flushing
       ;; delayed events
       (cxml:with-output-sink (cxml::*sink*)
	 (stop-buffering-and-flush value cxml::*sink*)))
      (list
       (case (first value)
	 (eval (eval (second value)))
	 (escaped (%emit-tagged-content (rest value) t))
	 (unescaped (%emit-tagged-content (rest value) nil))
	 (T (dolist (v value)
	      (%emit-tagged-content v escape)))))
      (T (%emit-tagged-content (princ-to-string value) escape)))))

(def-attribute-handler tal::when (tag)
  "ATTRIBUTE-HANDLER: Causes this tag to only appear when
the evaluated value of the attribute is not nil.
"
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::when))))
    `(when ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::unless (tag)
  "ATTRIBUTE-HANDLER: Causes this tag to only appear when
the evaluated value of the attribute is nil.
"
  (let ((value (read-tal-expression-from-string
		(pull-attrib-val! tag 'tal::unless))))
    `(unless ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::let (tag)
  "ATTRUBTE-HANDLER: Extend environment with a given list of bindings,
as for LET* form.

Example:
<div tal:let='foo 3'><div tal:content='$foo'/></div>

Goes to: <div><div>3</div></div>
"
  (iter
    (with let-attrib = (pull-attrib-val! tag 'tal::let))
    ;; returns a progn but we just need the list of sub exprs
    (with tal-expressions = (cdr (read-tal-expression-from-string let-attrib t)))
    (for (name value . rest) on tal-expressions by #'cddr)
    (collect (list name value) into let-bindings)
    (finally
     (return
       `(let* (,@let-bindings)
         (declare (ignorable ,@(mapcar #'car let-bindings)))
         ,(transform-lxml-form-in-scope tag))))))

(def-tag-handler tal::lisp (tag)
  "TAG-HANDLER: evaluate the body of the tag as lisp code."
  (let ((body (first (cddr tag))))
    `(%emit-tagged-content
      ,(read-tal-expression-from-string body T))
    ))

(def-tag-handler tal::loop (tag)
  "TAG-HANDLER: Loop across a list and repeat the children. On each
  iteration the environment is extended with the value

  tal:list should evaluate to a list
  tal:constant-list is parsed as a comma separated string

  <tal:loop tal:var='class' tal:list='$efficiencies'>
    <label class='map-control-item'>
      <img class='marker-icon ${$class}' />
      <span class='text' tal:content='$class'/>
      <input type='checkbox' name='filter-efficiency' item-type='${$class}' />
    </label>
  </tal:loop>

assuming that $efficiencies resolves to the list {foo,bar}.
    <label class='map-control-item'>
      <img class='marker-icon foo' />
      <span class='text' tal:content='foo'/>
      <input type='checkbox' name='filter-efficiency' item-type='foo' />
    </label>
    <label class='map-control-item'>
      <img class='marker-icon bar' />
      <span class='text' tal:content='bar'/>
      <input type='checkbox' name='filter-efficiency' item-type='bar' />
    </label>
"
  (let* ((name (intern-tal-string (pull-attrib-val! tag 'tal::var)))
	 (idx-string (pull-attrib-val! tag 'tal::idx))
	 (idx (when idx-string
		(intern (string-upcase idx-string) *expression-package*)))
	 (value-string (pull-attrib-val! tag 'tal::list))
	 (value (when value-string
		  (read-tal-expression-from-string value-string)))
	 (constant-list-string (pull-attrib-val! tag 'tal::constant-list))
	 (constant-list (when constant-list-string
			  (list* 'cl:list
			   (remove-if
			    (lambda (x) (or (null x) (zerop (length x))))
			    (cl-ppcre::split "\\s*,\\s*" constant-list-string))))))
    (when (and (null value)
	       (null constant-list))
      (tal-error "A tal:loop tag, must have a list to loop over
        (either tal:list, or tal:constant-list)"))
    (destructure-tag (tag)
      `(loop for ,name in ,(or value constant-list)
	     ,@(when idx
		 `(for ,idx upfrom 0))
	     append
	  (list ,@(transform-lxml-tree-in-scope tag-body))))))

(def-tag-handler tal:include (tag)
  "TAG-HANDLER: includes another template at this point in the file.
The template should be named by the tal:name attribute or by
tal:name-expression if it should be evaluated.

The path to the referenced template should be relative to the location
of the template it is included in.
  
The environment can be extended by including attributes in the
parameter namespace, or immediate children in the parameter namespace.

Example:
   <tal:include tal:name-expression='$tal-name' param:foo='foo' />
   <tal:include tal:name='other-template.tal' param:foo='foo'>
     <param:contents>
       <div>
         <span tal:when='$selected'>*</span>
         <span tal:content='$label'</span>
       </div>
     </param:contents>
   </tal>

The template other-template.tal will be evaluated with the additional
parameters of 'foo' and 'contents'.
"
  (let (augmented-env
	(template-name
	 (or
	  (pull-attrib-val! tag 'tal::name)
	  (awhen (pull-attrib-val! tag 'tal::name-expression)
	    (parse-tal-attribute-value it))
	  (tal-error "Missing TAL:NAME and TAL:NAME-EXPRESSION tags."))))
    (flet ((aug (var val)
	     (push var augmented-env)
	     (push val augmented-env))
	   (var (param)
	     `(quote ,(intern (string param) *expression-package*))))
      (destructure-tag (tag)	
	;; 1) grab all the attribute params
	(iter
	  (for (param value) in tag-attributes)
	  (if (eql (find-package :tal.include-params)
		   (symbol-package param))
	      (aug (var param) (parse-tal-attribute-value value))
	      (tal-warn "Ignoring attribute in TAL:INCLUDE: ~S (~S)."
			param (symbol-package param))))
	
	;; 2) grab all the body params
	(dolist (child tag-body)
	  (unless (stringp child)
	    (destructuring-bind (param-name attributes &rest body) child
		
	      (let ((type (or (cadr (find 'tal.include-params::type attributes
					  :key #'first))
			      (cadr (find 'tal::type attributes
					  :key #'first)))))
		(if (eql (find-package :tal.include-params)
			 (symbol-package param-name))
		    (aug (var param-name)
			 (cond
			   ((and type (string-equal type "string"))
			    `(buffer-xml-output () ,@(transform-lxml-tree body)))
			   (t
			    `(buffered-template-call
			      ,(compile-tal-parse-tree-to-lambda
				body *expression-package* T)
			      -tal-environment-))))
		    (tal-warn "Ignoring body tag in TAL:INCLUDE: ~S." param-name))))))
	;; 3) GO!
	;; TODO: Figure out the generator logic and make sure this still works.
	(with-unique-names (tname)
	  ;; some generators are not filesystem based and so shouldnt have
	  ;; a truename to merge (such as the test generator)
	  `(let ((,tname ,(if *tal-truename* 
			      (if (constantp template-name)
				  (merge-pathnames template-name *tal-truename*)
				  `(let ((tal-truename ,*tal-truename*))
				     (merge-pathnames ,template-name tal-truename)))
			      template-name)))
	     (call-template-with-tal-environment
	      ,*tal-generator* ,tname
	      (tal-env ,@(nreverse augmented-env))
	      )))))))

(def-tag-handler tal:def (tag)
  "TAG-HANDLER: creates an inline sub-template available for the duration
of the templates execution.

The template should be named by the tal:name attribute or by
tal:name-expression if it should be evaluated.


Example:
   <tal:def tal:name='deffed-template'>
       <div>
         <span tal:when='$selected'>*</span>
         <span tal:content='$label'</span>
       </div>
   </tal>

tal:def can also be used to create string constants when used with tal:type=\"string\"

Example:
   <tal:def tal:name='deffed-string' tal:type='string'>This is my string</tal>

  results in a let binding (deffed-string \"This is my string\")

"
  (let* ((template-name
	  (or
	   (pull-attrib-val! tag 'tal::name)
	   (awhen (pull-attrib-val! tag 'tal::name-expression)
	     (parse-tal-attribute-value it))
	   (tal-error "Missing TAL:NAME and TAL:NAME-EXPRESSION tags.")))
	 (type (pull-attrib-val! tag 'tal::type))
	 (template-variable (intern (string-upcase template-name) *expression-package*))
	 (*name-being-compiled*
	  (format nil "~A:~A" *name-being-compiled* template-name)))
    (destructure-tag (tag)
      (let* ((body-lambda
	      (if (string-equal type "string")
		  `(buffer-xml-output () ,@(transform-lxml-tree tag-body))
		  `(buffered-template-call
		       ,(compile-tal-parse-tree-to-lambda
			 tag-body *expression-package* T)
		       -tal-environment-))))
	(push (list template-variable body-lambda) *tal-defs*)
	nil ;; this tag returns nothing because we push the def up to the enclosing scope
	))))

(def-attribute-handler tal::in-package (tag)
  "ATTRIBUTE-HANDLER: sets the package in which lisp evaluation
happens."
  (let* ((pkg-string (pull-attrib-val! tag 'tal::in-package))
	 (pkg (or (find-package (read-from-string pkg-string))
		  (tal-error "No package named ~S found."
			     (read-from-string pkg-string)))))
    (let ((*expression-package* pkg))
      `(let ((*expression-package* ,pkg))
	 ,(transform-lxml-form tag)))))

(def-tag-handler tal::print-env (tag)
  "TAG-HANDLER: prints as a comment information about the
tal-environment where this tag was encountered"
  (declare (ignore tag))
    `(cxml:comment
      (with-output-to-string (out)
	(princ " TAL Env (searched top to bottom) : " out)
	(format out "~%*expression-package*: ~a" ,*expression-package*)
	(format out "~%===========================")
	(loop for bind-set on -tal-environment- by #'cddr
	      for key = (first bind-set)
	      for data = (second bind-set)
	      do
	   (format out "~%~15a : ~s" key data))
	(format out "~%===========================")
	(terpri out))))

(def-tag-handler tal::print-handlers (tag)
  "TAG-HANDLER: Lists (in a comment) the tag and attribute handlers
that are currently registered."
  (declare (ignore tag))
  `(cxml:comment
    (with-output-to-string (out)
      (format out "~%TAG-HANDLERS: ~s" *tal-tag-handlers*)
      (format out "~%ATTRIBUTE-HANDLERS: ~s "
	      *tal-attribute-handlers*)
      (terpri out))))

(def-tag-handler tal::describe-handler (tag)
  "TAG-HANDLER: Describes (in an comment) the tag or attribute handler named by the
attribute tal:name"
  (let ((*read-eval* nil))
    (let ((name (read-from-string
		 (pull-attrib-val! tag 'tal::name))))
      `(cxml:comment
	(with-output-to-string (*standard-output*)
	  (terpri)
	  (describe ',name)))
      )))

(def-tag-handler tal::describe-handlers (tag)
  "TAG-HANDLER: Describes (in an comment), all tags and attribute handlers that are known
   (in *tal-attribute-handlers* or *tal-tag-handlers*)"
  (declare (ignore tag))
  (let ((*read-eval* nil))
    `(cxml:comment
      (with-output-to-string (*standard-output*)
	(iter (for name in ',(append *tal-tag-handlers* *tal-attribute-handlers*))
	      (terpri)
	      (describe name)
	      )))))



;; Code added Copyright (c) 2010, Accelerated Data Works
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

