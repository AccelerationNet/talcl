;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * Compiling TAL from .tal files

(defclass tal-generator () ())

(defgeneric preprocess-tal (generator name)
  (:documentation "Returns the source code which a certain TAL
  name will expand into. Used for debugging tal code."))

(defgeneric load-tal (generator name))

(defgeneric template-truename (generator name))

(defclass file-system-generator (tal-generator)
  ((root-directories :initarg :root-directories :type list
		     :accessor root-directories)
   (cachep :initarg :cachep :accessor cachep :initform t)))

(defstruct (tal-template (:conc-name tal-template.))
  last-load-time
  file-name
  function)

(defparameter *tal-templates* (make-hash-table :test 'equal))

(defmethod template-truename ((generator file-system-generator) (name string))
  (template-truename generator (pathname name)))

(defmethod template-truename ((generator file-system-generator) (name pathname))
  (dolist (root (root-directories generator))
    (when-bind truename (probe-file (merge-pathnames name root))
       (return-from template-truename truename)))
  (return-from template-truename nil))

(defmethod load-tal ((generator file-system-generator) (name string))
  (load-tal generator (pathname name)))

(defmethod load-tal ((generator file-system-generator) (name pathname))
  (let ((file-name (template-truename generator name)))
    (assert file-name
	    (name)
	    "No template named ~S found." name)
    (unless (gethash file-name *tal-templates*)
      (setf (gethash file-name *tal-templates*) (make-tal-template :last-load-time 0
								   :function nil
								   :file-name file-name)))
    (lambda (environment generator)
      (let ((template (gethash file-name *tal-templates*))
	    (file-write-date (file-write-date file-name)))
        (when (or (not (cachep generator))
                  (< (tal-template.last-load-time template) file-write-date))
          (setf
           (tal-template.function template) (compile nil (preprocess-tal generator file-name))
           (tal-template.last-load-time template) file-write-date))
	(funcall (tal-template.function template) environment generator)))))

(defmethod preprocess-tal ((generator file-system-generator) (file-name string))
  (preprocess-tal generator (pathname file-name)))

(defmethod preprocess-tal ((generator file-system-generator) (name pathname))
  (with-tal-compilation-unit name
    (compile-tal-string-to-lambda (read-tal-file-into-string name))))

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
