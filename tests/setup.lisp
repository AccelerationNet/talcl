(defpackage :net.acceleration.talcl-test
    (:nicknames :talcl-test)
  (:use :common-lisp
	:net.acceleration.utils
	:it.bese.arnesi
	:iterate
	:talcl
	:lisp-unit
	:buildnode
	:bind))

(in-package :talcl-test)
(cl-interpol:enable-interpol-syntax)
(eval-always
  (unless (get-logger 'talcl-test::tal-log)
    (deflogger talcl-test::tal-log () :appender (arnesi:make-slime-repl-log-appender))))

(with-package-iterator (sym '(:talcl) :internal)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :talcl)
		     pkg)
	    (ignore-errors (import (list symbol) :talcl-test)))
	  (while more?))))

(defclass tal-test-generator (tal-generator)
  #.(slot-defs '(tals)))

(defmethod load-tal ((g tal-test-generator) name)
  (cdr (assoc name (tals g) :test #'equalp)))

(defmethod add-tal ((g tal-test-generator) (name t) (fn function))
  (setf (tals g) (remove name (tals g) :key #'car :test #'equalp))
  (push (cons name fn) (tals g)))

(defmethod add-tal ((g tal-test-generator) (name t) (fn string))
  (add-tal g name (talcl::compile-tal-string fn)))

(defmethod template-truename ((g tal-test-generator) name)
  name)

(defvar *test-generator* (make-instance 'tal-test-generator))

(defmacro adwtest (name (&rest args) &body body)
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name
     (tal-log.info #?"\nSTARTING Tal Test: ~S\n" ',name)
     (let* ((talcl::*tal-generator* *test-generator*)
	    (out (talcl::buffer-xml-output () ,@body)))
       (tal-log.info #?"\nTal Test: ~S \n-------------\n~a\n-------------\n" ',name out))))

(defun run-tests-with-debugging (&key tests suites)
  (let* ((lisp-unit::*use-debugger* T)
	 (tests (append (ensure-list tests)
			(iter (for suite in (ensure-list suites))
			      (appending (get suite :tests)))))
	 (out (with-output-to-string (s)
		(let ((lisp-unit::*lisp-unit-stream*
		       (make-broadcast-stream
			s
			(if (eql t lisp-unit::*lisp-unit-stream*)
			    *standard-output*
			    lisp-unit::*lisp-unit-stream*))))
		  (lisp-unit::run-test-thunks
		   (lisp-unit::get-test-thunks
		    (if (null tests) (get-tests *package*) tests)))))))
    (tal-log.info #?"\n ** TEST RESULTS ** \n-----------\n${out}\n------------\n")))
