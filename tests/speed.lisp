(in-package :talcl-test)

(asdf:load-system :talcl-examples)


(defmacro speed-test (name (&rest args) &body body)
  (let ((args (copy-list args)))
    (pushnew 'speed args)
    `(adwtest ,name (,@args)
      ,@body)))
  
(defmacro speed-test-iters (name iters (&rest args) &body body)    
  `(speed-test ,name (,@args)
    (iter (for i from 0 to ,iters)
      (progn ,@body)))
  )

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