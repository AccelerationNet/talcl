;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * Standard TAL handlers

(def-tag-handler tal::tal (tag)
  `(progn ,@(transform-lxml-tree (cdr tag))))

(def-attribute-handler tal::content (tag)
  "Becomes a TAL:REPLACE."
  (let ((value (getf (cdar tag) 'tal::content))
        (escape-html (getf (cdar tag) 'tal::escape-html)))
    (remf (cdar tag) 'tal::content)
    (remf (cdar tag) 'tal::escape-html)
    (destructuring-bind ((tag-name &rest attributes) &rest body) tag
      (declare (ignore body))
      (transform-lxml-form `((,tag-name ,@attributes)
                             ((CONTENT-DUMMY tal::replace ,value tal::escape-html ,escape-html) "DUMMY"))))))

(def-attribute-handler tal::content-as-is (tag)
  "Becomes a TAL:REPLACE."
  (let ((value (getf (cdar tag) 'tal::content-as-is)))
    (remf (cdar tag) 'tal::content-as-is)
    (destructuring-bind ((tag-name &rest attributes) &rest body) tag
      (declare (ignore body))
      (transform-lxml-form `((,tag-name ,@attributes)
                             ((CONTENT-DUMMY tal::replace ,value tal::escape-html "nil") "DUMMY"))))))

(def-attribute-handler tal::replace (tag)
  (let ((value (read-tal-expression-from-string (getf (cdar tag) 'tal::replace)))
        (escape (if-bind escape (getf (cdar tag) 'tal::escape-html)
		  ;; if they supplied a vlaue then use it (either nil or t or whatever)
                  (read-tal-expression-from-string escape)
                  ;; no value supplied, default to T
                  t)))
    (remf (cdar tag) 'tal::replace)
    (remf (cdar tag) 'tal::escape-html)
    (if escape
	`(<:as-html ,value)
        `(<:as-is ,value))))

(def-attribute-handler tal::when (tag)
  (let ((value (read-tal-expression-from-string (getf (cdar tag) 'tal::when))))
    (remf (cdar tag) 'tal::when)
    `(when ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::unless (tag)
  (let ((value (read-tal-expression-from-string (getf (cdar tag) 'tal::unless))))
    (remf (cdar tag) 'tal::unless)
    `(unless ,value
       ,(transform-lxml-form tag))))

(def-attribute-handler tal::dolist (tag)
  "On each iteration the environment is extended with the value in
  the value passed to DOLIST."
  (let ((value (read-tal-expression-from-string (getf (cdar tag) 'tal::dolist))))
    (remf (cdar tag) 'tal::dolist)
    (with-unique-names (loop-item-sym)
      `(dolist (,loop-item-sym ,value)
         (let ((-tal-environment- (extend-environment ,loop-item-sym -tal-environment-)))
           ,(transform-lxml-form tag))))))

(def-attribute-handler tal::let (tag)
  "Extend environment with a given list of bindings, as for LET form."
  (let ((bindings
         (loop
            for (name value) in (read-tal-expression-from-string (getf (cdar tag) 'tal::let))
            collect `(cons ',name ,value))))
    (remf (cdar tag) 'tal::let)
    `(let ((-tal-environment- (extend-environment (list (list ,@bindings)) -tal-environment-)))
       ,(transform-lxml-form tag))))

(def-tag-handler tal::lisp (tag)
  (read-tal-expression-from-string (first (cdr tag))))

(def-tag-handler tal:include (tag)
  (let (template-name)
    (cond
      ((getf (cdar tag) 'tal::name)
       (setf template-name (getf (cdar tag) 'tal::name))
       (remf (cdar tag) 'tal::name))
      ((getf (cdar tag) 'tal::name-expression)
       (setf template-name (parse-tal-attribute-value (getf (cdar tag) 'tal::name-expression)))
       (remf (cdar tag) 'tal::name-expression))
      (t
       (error "Missing TAL:NAME and TAL:NAME-EXPRESSION tags.")))
    (with-collector (augmented-env)
      ;; 1) grab all the attribute params
      (loop
	 for (param value) on (cdar tag) by #'cddr
	 if (eql (find-package :it.bese.yaclml.tal.include-params)
                 (symbol-package param))
	   do (augmented-env `(quote ,(intern (string param) *expression-package*))
                             (read-tal-expression-from-string value))
	 else
	   do (warn "Ignoring attribute in TAL:INCLUDE: ~S (~S)." param (symbol-package param)))
      ;; 2) grab all the body params
      (dolist (child (cdr tag))
        (unless (stringp child)
          (destructuring-bind ((param-name) . body) 
              child
            (if (eql (find-package :it.bese.yaclml.tal.include-params)
                     (symbol-package param-name))
                (augmented-env `(quote ,(intern (string param-name) *expression-package*))
                               `(with-output-to-string (*yaclml-stream*)
                                  ,@(mapcar #'transform-lxml-form body)))
                (warn "Ignoring body tag in TAL:INCLUDE: ~S." param-name)))))
      ;; 3) GO!
      (with-tal-compile-environment (generator)
        `(funcall (load-tal ,generator ,(if (constantp template-name)
                                            (merge-pathnames template-name *tal-truename*)
                                            `(let ((tal-truename ,*tal-truename*))
                                               (merge-pathnames ,template-name tal-truename))))
                  (extend-environment (tal-env ,@(augmented-env)) -tal-environment-)
                  ,generator)))))

(def-attribute-handler tal::in-package (tag)
  (let ((*expression-package* (or (find-package (read-from-string (getf (cdar tag) 'tal::in-package)))
                                  (error "No package named ~S found." (read-from-string (getf (cdar tag) 'tal::in-package))))))
    (remf (cdar tag) 'tal::in-package)
    (transform-lxml-form tag)))

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

