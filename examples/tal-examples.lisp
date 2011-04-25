(in-package :talcl)

;; This file builds html dom nodes for the examples so make sure
;; we have that package available

(asdf:oos 'asdf:load-op :buildnode-xhtml)

;; Generators help locate templates files on disk, this one
;; will locate files for the system in the "examples" dir

(defvar *example-generator*
  (make-instance 'talcl:caching-file-system-generator
		 :root-directories (list (asdf:system-relative-pathname
					  :talcl #p"examples/"))))

;; an example of building a dom snippet, to be spliced into the template

(defun example-footer-menu ()
  (xhtml:ul ()
    (iter (for i from 0 to 4)
	  (for name = (format nil "~D footer list item" i))
	  (collect (xhtml:li `(:title ,name) name )))))

;; This will print out the 

(defun process-example-window-to-string ()
  (let ((net.acceleration.buildnode:*html-compatibility-mode* t)
	(doc (buildnode:with-html-document
	       (tal-processing-instruction
		*example-generator* "window.tal"
		(talcl:tal-env 'page-title "My Example Page"
			       'header-dom-id "header"
			       'body (xhtml:div ()
				       (xhtml:h3 () "Example DOM body content")
				       (xhtml:p () "Lorem Ipsum"))

			       'escaped-html-string "<span >Escaped HTML</span>"
			       'unescaped-html-string "<span >Unescaped HTML</span>"
			       )))))
    ;;(break "~A" doc)
    (document-to-string doc))
  )