(in-package :talcl-test)

(asdf:load-system :talcl-examples)

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defmacro log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((gmessage (gensym "GMESSAGE-")))
      `(let ((,gmessage ,message))
	 (flet ((msg (&optional tag)
		  (format nil "~A ~a"
			  tag ,gmessage)))
	   (,log-name (msg "BEGIN") ,@args)
	   (multiple-value-prog1	     
	       (progn ,@body)
	     (,log-name (msg "  END") ,@args))))))

  (defmacro time-and-log-around ((log-name message &rest args) &body body)
    "Logs the beginning and end of a body.  ARGS are evaluated twice"
    (let  ((trace-output (gensym "TRACE-OUTPUT-")))
      `(let (,trace-output) ;;leave nil so the first log call doesn't print an extra newline
	 (log-around (,log-name ,(concatenate 'string message "~@[~%~a~]") ,@args ,trace-output)
	   (setf ,trace-output
		 (make-array 10 :element-type 'character :adjustable T :fill-pointer 0))
	   (with-output-to-string (*trace-output* ,trace-output)
	     (time (progn ,@body)))))))
  
  (defmacro speed-test (name (&rest args) &body body)
    (let ((args (copy-list args)))
      (pushnew 'speed args)
      `(adwtest ,name (,@args)
	 (time-and-log-around (tal-log.info "Running Test ~A" ',name)
	  ,@body))
      ))
  
  (defmacro speed-test-iters (name iters (&rest args) &body body)    
    `(speed-test ,name (,@args)
       (iter (for i from 0 to ,iters)
	     (progn ,@body)))
    ))

(defun example-window-env ()
  (talcl:tal-env
   'talcl::page-title "My Example Page"
   'talcl::header-dom-id "header"
   'talcl::body (xhtml:div ()
		  (xhtml:h3 () "Example DOM body content")
		  (xhtml:p () "Lorem Ipsum"))

   'talcl::escaped-html-string "<span >Escaped HTML</span>"
   'talcl::unescaped-html-string "<span >Unescaped HTML</span>"
   'talcl::print-help nil
   ))


(speed-test-iters build-into-dom-speed-test 1000 ()
  (talcl:document-to-string
   (buildnode:with-xhtml-document
     (talcl:tal-template-content
      *example-generator* "window.tal"
      (example-window-env)))))

(speed-test-iters build-into-dom-processing-node-speed-test 1000 ()
  (talcl:document-to-string
   (buildnode:with-xhtml-document 
     (talcl:tal-processing-instruction
      *example-generator* "window.tal" (example-window-env)))))