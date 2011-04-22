;;;; -*- lisp -*-

(in-package :talcl)

;;;;;;;;;;;;;;; START BASIC UTILS - COPIED FROM ARNESI (and my internal util lib)
;;;;;;;;;;;;;;; so I dont depend on a huge lib for four functions

(defun dos-safe-read-line (stream &optional (eof-error-p t) eof-value recursive-p)
  "readline that can read unix or dos lines"
  (let ((line (read-line stream eof-error-p eof-value recursive-p)))
    (if (stringp line)
	(delete #\return line)
	line)))

(defun ensure-list (thing)
  "Returns THING as a list.

If THING is already a list (as per listp) it is returned,
otherwise a one element list containing THING is returned."
  (if (listp thing)
      thing
      (list thing)))

(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  "Just like when except VAR will be bound to the
  result of TEST in BODY."
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  "Just like when expect the symbol IT will be
  bound to the result of TEST in BODY."
  `(when-bind it ,test ,@body))


(defun read-string-from-file (pathname &key (buffer-size 4096)
                                            (element-type 'character)
                                            (external-format :us-ascii))
  "Return the contents of PATHNAME as a fresh string.

The file specified by PATHNAME will be read one ELEMENT-TYPE
element at a time, the EXTERNAL-FORMAT and ELEMENT-TYPEs must be
compatible.

The EXTERNAL-FORMAT parameter will be passed to
ENCODING-KEYWORD-TO-NATIVE, see ENCODING-KEYWORD-TO-NATIVE to
possible values."
  (with-input-from-file
      (file-stream pathname :external-format (encoding-keyword-to-native external-format))
    (with-output-to-string (datum) 
      (let ((buffer (make-array buffer-size :element-type element-type)))
	(loop for bytes-read = (read-sequence buffer file-stream)
	      do (write-sequence buffer datum :start 0 :end bytes-read)
	      while (= bytes-read buffer-size))))))

(defmacro with-unique-names ((&rest bindings) &body body)
  "Evaluate BODY with BINDINGS bound to fresh unique symbols.

Syntax: WITH-UNIQUE-NAMES ( [ var | (var x) ]* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar (lambda (binding)
                   (check-type binding (or cons symbol))
                   (destructuring-bind (var &optional (prefix (symbol-name var)))
                       (if (consp binding) binding (list binding))
                     (check-type var symbol)
                     `(,var (gensym ,(concatenate 'string prefix "-")))))
                 bindings)
     ,@body))

(defmacro rebinding (bindings &body body)
  "Bind each var in BINDINGS to a gensym, bind the gensym to
var's value via a let, return BODY's value wrapped in this let.

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical
environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (car (if (consp binding) binding (list binding)))
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let* ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                               ,,@body))))))

(defun find-file-in-directories (name root-directories)
  "Find the given name in a list of directories, ensuring that the
  resulting file is actually contained in those directories;
  i.e. prevent escaping the bounds via ../../../"
  (let* ((name (pathname name))
	 (dir-list (pathname-directory name))
	 (rel-p (or (null dir-list)
		    (eql :relative (first dir-list)))))

    (when (and (not rel-p)
	       (not (probe-file name)))
      ;;absolute name for missing file
      (return-from find-file-in-directories nil))

    (dolist (root root-directories)
      (let ((root (pathname root)))
	(if rel-p
	    (let ((merged (eliminate-..-in-path
			   (merge-pathnames name root))))
	      (awhen (probe-file merged)
		;;found a file, now check that it is in the roots. 
		(return-from find-file-in-directories
		  (find-file-in-directories merged root-directories))))
	    (when (starts-with dir-list (pathname-directory root) :test #'equal)
	      (return-from find-file-in-directories (probe-file name))))))))

;;;;;;;;;;;;;;; END BASIC UTILS


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

(defmacro with-this-sink ((sink) &body body)
  `(let ((cxml::*sink* ,sink)
	 (cxml::*current-element* nil)
	 (cxml::*unparse-namespace-bindings* cxml::*initial-namespace-bindings*)
	 (cxml::*current-namespace-bindings* nil))
     ,@body
     ))

(defun buffered-template-call ( template-function env)
  (let ((sink (make-instance 'buffering-sink :buffering T)))
    (with-this-sink (sink)
      (talcl::%call-template-with-tal-environment template-function env)
      sink)))

(defun %call-template-with-tal-environment (tal-fn env)
  "This will call a template-fn with all the tal-environment variables
   bound into the lisp dynamic environment."
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
  (%call-template-with-tal-environment (load-tal generator template) env))

(defmacro buffer-xml-output (() &body body)
  "buffers out sax:events to a sting

   xml parameters like <param:foo param:type=\"string\"><div>bar</div></param:foo>
       are requested to be strings (presumably for string processing)
  "
  (with-unique-names (out-str)
    `(with-output-to-string (,out-str)
       (let ((cxml::*sink* (cxml::make-character-stream-sink ,out-str))
	     (cxml::*current-element* nil)
	     (cxml::*unparse-namespace-bindings* cxml::*initial-namespace-bindings*)
	     (cxml::*current-namespace-bindings* nil))
	 (setf (cxml::sink-omit-xml-declaration-p cxml::*sink*) T)
	 (sax:start-document cxml::*sink*)
	 ,@body  
	 (sax:end-document cxml::*sink*)))))

(defun run-template ( generator template env )
  "Runs a tal template returning the string produced"
  (buffer-xml-output ()
    (call-template-with-tal-environment generator template env)))

(defun run-template-fn ( fn env )
  "Runs a tal template returning the string produced"
  (buffer-xml-output ()
    (%call-template-with-tal-environment fn env)))

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
