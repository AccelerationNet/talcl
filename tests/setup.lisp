
(defpackage :net.acceleration.talcl-test
    (:nicknames :talcl-test)
  (:use :common-lisp
	:iterate
	:talcl
	:lisp-unit)
  (:shadow :run-tests))

(in-package :talcl-test)


(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defun tal-log.info (message &rest args)
  (format *standard-output* "~&")
  (log-time (get-universal-time) *standard-output*)
  (apply #'format *standard-output* message args)
  (format *standard-output* "~%"))

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
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (alexandria:ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name
     (tal-log.info "~%STARTING Tal Test: ~S~%" ',name)
     (let* ((talcl::*tal-generator* *test-generator*)
	    (out (talcl::buffer-xml-output () ,@body)))
       (when (plusp (length out))
	 (tal-log.info
	  "~%Tal Test: ~S ~%-------------~%~a~%-------------~%" ',name out)
	 ))))

(defun run-tests (&key suites tests (use-debugger T))
  (let* ((*package* (find-package :talcl-test))
         (lisp-unit:*print-failures* t)
         (lisp-unit:*print-errors* t)
	 (lisp-unit::*use-debugger* use-debugger)
	 (tests (append (alexandria:ensure-list tests)
			(iter (for suite in (alexandria:ensure-list suites))
                          (appending (get suite :tests)))))
         (actual-std-out *standard-output*)
	 (out (with-output-to-string (s)
		(let ((*standard-output*
                        (make-broadcast-stream s actual-std-out)))
                  (if (null tests)
                      (lisp-unit::%run-all-thunks)
                      (lisp-unit::%run-thunks tests))))))
    (format *standard-output*
     "~&~% ** TEST RESULTS: TALCL ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
     out)))
