;;;; this file is for packages used by the tal system
;;;; see ucw:*uri-to-package*

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
