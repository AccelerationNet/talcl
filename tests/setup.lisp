
(defpackage :net.acceleration.talcl-test
    (:nicknames :talcl-test)
  (:use :common-lisp
	:iterate
	:talcl
	:lisp-unit2)
  (:shadow :run-tests))

(in-package :talcl-test)


(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun tal-log.info (message &rest args)
  (format lisp-unit2:*test-stream* "~&")
  (log-time (get-universal-time) lisp-unit2:*test-stream*)
  (apply #'format lisp-unit2:*test-stream* message args)
  (format lisp-unit2:*test-stream* "~%"))

(with-package-iterator (sym '(:talcl) :internal)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :talcl)
		     pkg)
	    (ignore-errors
	      (unintern symbol :talcl-test)
	      (import (list symbol) :talcl-test)))
	  (while more?))))

(defclass tal-test-generator (tal-generator)
  ((tals :accessor tals :initarg :tals :initform nil)))

(defmethod load-tal ((g tal-test-generator) name)
  (cdr (assoc name (tals g) :test #'equalp)))

(defmethod add-tal ((g tal-test-generator) (name t) (fn function))
  (setf (tals g) (remove name (tals g) :key #'car :test #'equalp))
  (push (cons name fn) (tals g)))

(defmethod add-tal ((g tal-test-generator) (name t) (fn string))
  (add-tal g name (talcl::compile-tal-string fn)))

(defmethod add-tal ((g tal-test-generator) (name t) (pth pathname))
  (add-tal g name (talcl::compile-tal-file pth)))

(defmethod template-truename ((g tal-test-generator) name)
  name)

(defvar *test-generator* (make-instance 'tal-test-generator))
(defvar *example-generator*
  (make-instance 'talcl:caching-file-system-generator
		 :root-directories (list (asdf:system-relative-pathname
					  :talcl #p"examples/"))))

(defmacro adwtest (name (&rest args) &body body)
  `(lisp-unit2:define-test ,name (:tags '(,@args))
     (let* ((talcl::*tal-generator* *test-generator*)
	    (out (talcl::buffer-xml-output () ,@body)))
       (when (plusp (length out))
	 (tal-log.info
	  "~%~S OUTPUT: ~%-------------~%~a~%-------------~%" ',name out)
	 ))))

(defun run-tests (&key suites tests)
  (format lisp-unit2:*test-stream* "~%")
  (prog1
      (let* ((*package* (find-package :talcl-test))
             (res (lisp-unit2:run-tests
                   :tests tests
                   :tags suites
                   :name :talcl
                   :run-contexts #'lisp-unit2:with-summary-context
                   )))
    
        (or
         (when talcl::*rely-on-warnings-for-variables*
           (let (talcl::*rely-on-warnings-for-variables*)
             (list res (rerun-tests res))))
         res))
    (format lisp-unit2:*test-stream* "~%"))
  )
