;;;; this file is for packages used by the tal system
;;;; see ucw:*uri-to-package*

(defpackage :net.acceleration.talcl
    (:use :common-lisp :arnesi :iterate)
  (:nicknames :talcl)
  (:export
   #:def-attribute-handler
   #:def-tag-handler
   #:read-tal-expression-from-string
   #:tal-generator
   #:file-system-generator
   #:caching-file-system-generator
   #:load-tal
   #:template-truename
   #:root-directories
   #:*uri-to-package*
   #:make-interner
   #:transform-lxml-form
   #:transform-lxml-tree
   #:pull-attrib-val!
   #:tal-env
   ))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (when (find-package :it.bese.yaclml.tal)
    (delete-package :it.bese.yaclml.tal)))

(defpackage :net.common-lisp.project.bese.tal.core
  (:use)
  (:documentation "An HTML template authoring library.")
  (:nicknames :tal)
  (:export #:tal 
           #:content
           #:replace
           #:when
           #:dolist
           #:include
           #:in-package
	   #:loop))

(defpackage :net.common-lisp.project.bese.tal.include-params
  (:use)
  (:nicknames :tal.include-params))
