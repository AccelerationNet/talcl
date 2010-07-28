;; -*- lisp -*-

(in-package :ucw)

;;;; * Compiling TAL from .tal files

(defclass tal-generator () ())

(defgeneric load-tal (generator name))

(defgeneric template-truename (generator name))


(defclass file-system-generator (tal-generator)
  ((root-directories :initarg :root-directories :type list
		     :accessor root-directories)))

(defclass caching-file-system-generator (file-system-generator)
  ((cache :accessor cache :initform (make-hash-table :test 'equal))))

(defparameter *tal-templates* (make-hash-table :test 'equal))

(defmethod template-truename ((generator file-system-generator) name)
  (find-file-in-directories name (root-directories generator)))

(defmethod load-tal ((generator file-system-generator) (name string))
  (load-tal generator (pathname name)))

(defmethod load-tal ((generator file-system-generator) (name pathname))
  (let ((file-name (template-truename generator name)))
    (assert file-name (name)
	    "No template named ~S found with generator ~a."
	    name generator)
    (let ((*tal-generator* generator))
      (compile-tal-file file-name))))

(defstruct (tal-template (:conc-name tal-template.))
  last-load-time
  file-name
  function)

(defun %get-tal-template-fn (file-name generator)
  (let ((template (make-tal-template :last-load-time (file-write-date file-name)
				     :function (let ((*tal-generator* generator))
						 (compile-tal-file file-name))
				     :file-name file-name)))
    (lambda (environment)
      (let* ((file-name (tal-template.file-name template))
	     (file-write-date (file-write-date file-name)))
	(when (< (tal-template.last-load-time template) file-write-date)
	  (let ((fun (let ((*tal-generator* generator))
		       (compile-tal-file file-name))))
	    (setf (tal-template.function template) fun
		  (tal-template.last-load-time template) file-write-date)))

	(funcall (tal-template.function template) environment)))))

(defmethod load-tal ((generator caching-file-system-generator)
		     name)
  (when (null name)
    (error "Can't load-tal for empty(NIL) name."))
  
  (let* ((hash-val (gethash name (cache generator)))
	 (template
	     (or hash-val
		 (if (and (pathnamep name)
			  (probe-file name))
		     (%get-tal-template-fn name generator)
		     (aif (template-truename generator name)
			  (load-tal generator it)
			  (error "Can't find template named ~s for generator ~a."
				 name generator))))))
    
    (prog1 template
      (unless (eql hash-val template)
	(setf (gethash name (cache generator)) template)))))




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
