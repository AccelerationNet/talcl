
(defpackage :it.bese.yaclml.tal
  (:use)
  (:documentation "An HTML template authoring library.")
  (:nicknames :tal)
  (:export #:tal 
           #:content
           #:replace
           #:when
           #:dolist
           #:include
           #:in-package))

(defpackage :it.bese.yaclml.tal.include-params
  (:use))