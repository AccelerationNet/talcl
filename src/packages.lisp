;;;; this file is for packages used by the tal system
;;;; see ucw:*uri-to-package*

(defpackage :net.acceleration.talcl
    (:use :common-lisp :iterate)
  (:nicknames :talcl)
  (:import-from :buildnode :buffer-xml-output)
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
   #:call-template-with-tal-environment
   #:run-template
   #:run-template-fn
   #:destructure-tag
   #:tag-name
   #:tag-attributes
   #:tag-body
   #:tal-runtime-condition
   #:set-value ;; restart
   #:with-missing-value-handler
   #:compile-tal-string
   #:compile-tal-file
   #:FIND-FILE-IN-DIRECTORIES

   ;; dom stuff
   #:template-processing-sink
   #:make-template-processing-sink
   #:tal-processing-instruction

   #:document-to-string
   #:document-to-stream
   #:tal-template-content

   )
  (:shadow ))

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
	   #:loop
	   #:def
	   #:dom-content))

(defpackage :net.common-lisp.project.bese.tal.include-params
  (:use)
  (:nicknames :tal.include-params))

(pushnew :talcl *features*)