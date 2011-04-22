(in-package :talcl)

(defvar *example-generator*
  (make-instance 'talcl:file-system-generator
		 :root-directories (list (asdf:system-relative-pathname
					  :talcl #p"examples/"))))

(defun example-footer-menu ()
  (xhtml:ul ()
    (iter (for i from 0 to 4)
	  (for name = (format nil "~D footer list item" i))
	  (collect (xhtml:li `(:title ,name) name )))))

(defun process-example-window-to-string ()
  (let (;;(NET.ACCELERATION.BUILDNODE:*HTML-COMPATIBILITY-MODE* T)
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