;;;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * TAL Template Environments

(defmacro with-tal-compilation-unit (pathname &body body)
  (rebinding (pathname)
    `(let ((*tal-truename* (truename ,pathname)))
      ,@body)))

(defun read-tal-file-into-string (pathname)
  (read-string-from-file pathname :external-format :utf-8))

;;;; TAL environments are simply lists of binding-sets, a binding set
;;;; can be either a hash table, an object or an alist.

;;;; TAL Environment protocol

(defmacro tal-value (name)
  "Get the tal variable called NAME from -TAL-ENVIRONMENT-"
  `(lookup-tal-variable ,name -tal-environment-))

(defgeneric lookup-tal-variable (name environment)
  (:documentation "Return the value assciated with NAME (a
  symbol) in the environment ENVIRONMENT."))

(defgeneric (setf lookup-tal-variable) (value name environment))

(defgeneric fetch-tal-value (name binding)
  (:documentation "Return the value associated with NAME in the
  binding set BINDING."))

(defgeneric (setf fetch-tal-value) (value name binding))

(defun extend-environment (new-environment environment)
  "Create a new environment with all the bindings in NEW-ENVIRONMENT and ENVIRONMENT.

Any bindings in NEW-ENVIRNOMENT shadow (on successive calls to
LOOKUP-TAL-VARIABLE) other bindings currently present in
ENVIRONMENT."
  ;; return a new list containing all the binding-sets of
  ;; new-environment and then all the binding sets of environment.
  (append new-environment environment))

;;;; Standard Environment

(defun make-standard-tal-environment (&rest binding-sets)
  "Returns an environment consisting of BINDING-SETS.

Each binding set can be an alist, an object, a hash table, or any
object for which the a method on LOOKUP-TAL-VARIABLE has been
defined.

See alse: TAL-ENV"
  (copy-list binding-sets))

(defmethod lookup-tal-variable (name (env list))
  "Return the value associated with NAME in the environment
  ENV. ENV is represetend as a list of binding sets."
  (loop
     for bind-set in env
     do (multiple-value-bind (value found-p)
            (fetch-tal-value name bind-set)
          (when found-p
            (return-from lookup-tal-variable (values value t)))))
  (values nil nil))

(define-condition setf-tal-variable-error (error)
  ((variable-name :initarg :variable-name)
   (environment :initarg :environment))
  (:report (lambda (c s)
	     (format s "Error setting the tal variable ~S~:[.~; in the environment ~S.~]"
		       (slot-value c 'variable-name)
		       (slot-boundp c 'environment)
		       (slot-value c 'environment)))))

(define-condition unfound-tal-variable (setf-tal-variable-error)
  ()
  (:report (lambda (c s)
	     (format s "Attempting to set unknown tal variable ~S~:[.~; in the environment ~S.~]"
		       (slot-value c 'variable-name)
		       (slot-boundp c 'environment)
		       (slot-value c 'environment)))))

(define-condition unsettable-tal-variable (setf-tal-variable-error)
  ()
  (:report (lambda (c s)
	     (format s "The tal variable ~S is unsettable~:[.~; in the environment ~S.~]"
		       (slot-value c 'variable-name)
		       (slot-boundp c 'environment)
		       (slot-value c 'environment)))))

(defmethod (setf lookup-tal-variable) (value name (env list))
  (loop
     for bind-set in env
     do (multiple-value-bind (old-value found-p)
            (fetch-tal-value name bind-set)
	  (declare (ignore old-value))
          (when found-p
	    (return-from lookup-tal-variable
	      (setf (fetch-tal-value name bind-set) value)))))
  (error 'unfound-tal-variable :variable-name name :environment env))

(defun tal-env (&rest pairs)
  "Creates a fresh tal environment from the plist PAIRS."
  (list
   (iterate (for (key value) :on pairs :by #'cddr)
            (collect (cons key value)))))

;;;; Assoc list binding set

(defmethod fetch-tal-value (name (binding-set list))
  (let ((cons (assoc name binding-set :test #'eql)))
    (if cons
        (values (cdr cons) t)
        (values nil nil))))

(defmethod (setf fetch-tal-value) (value name (binding-set list))
  (declare (ignore value))
  (error 'unsettable-tal-variable :variable-name name :environment binding-set))

;;;; Object binding sets

(defmethod fetch-tal-value (name (obj standard-object))
  (if (and (slot-exists-p obj name)
           (slot-boundp obj name))
      (values (slot-value obj name) t)
      (values nil nil)))

(defmethod (setf fetch-tal-value) (value name (obj standard-object))
  (if (slot-exists-p obj name)
      (setf (slot-value obj name) value)
      (error 'unfound-tal-variable :variable-name value :environment obj)))

;;;; Hash table binding sets

(defmethod fetch-tal-value (name (ht hash-table))
  (gethash name ht))

(defmethod (setf fetch-tal-value) (value name (ht hash-table))
  (setf (gethash name ht) value))

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
