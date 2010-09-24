;;;; -*- lisp -*-

(in-package :talcl)

;;;; * TAL Template Environments
(defvar *tal-truename* nil
  "The truename of the tal file being compiled.")
(defvar *tal-generator* nil
  "The generator to be used during compilation.")

(defmacro with-tal-compilation-unit (pathname &body body)
  (rebinding (pathname)
    `(let ((*tal-truename* (truename ,pathname)))
      ,@body)))

(defun read-tal-file-into-string (pathname)
  (or
   (ignore-errors (read-string-from-file pathname :external-format :utf-8))
   (ignore-errors (read-string-from-file pathname :external-format :latin-1))
   (ignore-errors (read-string-from-file pathname :external-format :ascii))
   (error "Failed to load template content in utf-8, latin-1 or ascii ~a" pathname)))


(defun %call-template-with-tal-environment (tal-fn env)
  "This will call a template-fn with all the tal-environment variables
   bound into the lisp dynamic environment."
  
  ;; Why first... doesnt make much sense but we seem
  ;; to be storing the alist in a list
  (iter (for (k v . rest) on env by #'cddr)
	(collect k into keys)
	(collect v into values)
	(finally 
	 (progv keys values
	   ;; pass env to funcall to assist debugging
	   (return (funcall tal-fn env))
	   ))))

(defun call-template-with-tal-environment (generator template env)
  "This will call a template with all the tal-environment variables
   bound into the lisp dynamic environment."
  
  ;; Why first... doesnt make much sense but we seem
  ;; to be storing the alist in a list
  (%call-template-with-tal-environment (load-tal generator template) env))

(defun tal-env (&rest pairs)
  "Creates a fresh tal environment from the plist PAIRS."
  ;; currently just an alias for list
  pairs)


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
