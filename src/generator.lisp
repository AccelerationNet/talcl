;; -*- lisp -*-

(in-package :talcl)

;;;; * Compiling TAL from .tal files

(defvar *log* nil "A log-fn that accepts a string and writes it to the log")

(defun tal-log (message &rest args)
  (when *log*
    (funcall *log* (apply #'format nil message args))))

(defclass tal-generator ()
  ((log-fn :accessor log-fn :initarg :log-fn :initform nil
           :documentation "A log-fn that accepts a string and writes it to the log")))

(defgeneric load-tal (generator name))

(defgeneric template-truename (generator name))


(defclass file-system-generator (tal-generator)
  ((root-directories :initarg :root-directories :type list
		     :accessor root-directories))
  (:documentation "When given a list of root directories it will
  search for templates in those directories."))

(defclass caching-file-system-generator (file-system-generator)
  ((cache :accessor cache :initform (make-hash-table :test 'equal)))
  (:documentation "Similar to a file-system-generator it generates out
  of some root directories, however it keeps a cache of the compiled
  lisp functions. The function you get back from it checks the file's
  date-modified and automatically recompiles at need."))

(defparameter *tal-templates* (make-hash-table :test 'equal))

(defmethod template-truename ((generator file-system-generator) name
                              &aux (*log* (log-fn generator)))
  (tal-log "finding truename of template: ~s ~s" generator name)
  (find-file-in-directories
   name (root-directories generator)))

(defmethod load-tal ((generator file-system-generator) (name string)
                     &aux (*log* (log-fn generator)))
  (tal-log "Loading tal template: ~s ~s" generator name)
  (load-tal generator (pathname name)))

(defmethod load-tal ((generator file-system-generator) (name pathname)
                     &aux (*log* (log-fn generator)))
  (let ((file-name (template-truename generator name)))
    (tal-log "Loading with truename of template: ~s ~s" generator file-name)
    (assert file-name (name)
	    "No template named ~S found with generator ~a."
	    name generator)
    (let ((*tal-generator* generator))
      (tal-log "About to compile: ~s ~s" generator file-name)
      (compile-tal-file file-name))))

(defstruct (tal-template (:conc-name tal-template.))
  last-load-time
  file-name
  function)

(defun %get-tal-template-fn (file-name generator
                             &aux (*log* (log-fn generator)))
  (let ((template (make-tal-template :last-load-time (file-write-date file-name)
				     :function (let ((*tal-generator* generator)
                                                     (*log* (log-fn generator)))
						 (compile-tal-file file-name))
				     :file-name file-name)))
    (lambda (environment
        &aux (*log* (log-fn generator)))
      (let* ((file-name (tal-template.file-name template))
	     (file-write-date (file-write-date file-name)))
        (tal-log "Tal template: ~s ~s" file-name file-write-date)
	(when (< (tal-template.last-load-time template) file-write-date)
	  (let ((fun (let ((*tal-generator* generator))
                       (tal-log "Tal Recompiling: ~s ~s ~s"
                                file-name file-write-date (tal-template.last-load-time template))
		       (compile-tal-file file-name))))
            (tal-log "Tal Got new FN for: ~s ~s ~s"
                     file-name file-write-date fun)
	    (setf (tal-template.function template) fun
		  (tal-template.last-load-time template) file-write-date)))
        (tal-log "Tal Executing Tal-Template: ~s ~s ~s"
                 file-name template environment)
	(prog1 (funcall (tal-template.function template) environment)
          (tal-log "Tal Finished executing template:~S" file-name))))))

(defmethod load-tal ((generator caching-file-system-generator)
		     name
                     &aux (*log* (log-fn generator)))
  (when (null name) (error "Can't load-tal for empty(NIL) name."))
  (tal-log "Tal Loading cached ~S ~S" name generator)
  (let* ((hash-val (gethash name (cache generator)))
         true-name
	 (template
           (cond (hash-val
                  (tal-log "Tal Found cached copy ~S ~S" name hash-val)
                  hash-val)
                 ((and (pathnamep name) (probe-file name))
                  (tal-log "Tal uncached template exists so compile it ~S" name)
                  (%get-tal-template-fn name generator))
                 ((setf true-name (template-truename generator name))
                  (tal-log
                   "Tal found true-name for template, so load that ~S" true-name)
                  (load-tal generator true-name))
                 (T (error "Can't find template named ~s for generator ~a."
                           name generator))
		 )))
    (prog1 template
      (unless (eql hash-val template)
	(setf (gethash name (cache generator)) template)))))


