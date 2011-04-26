(in-package :talcl)

;; This file builds html dom nodes for the examples so make sure
;; we have that package available
(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (asdf:oos 'asdf:load-op 'buildnode-xhtml))

;; Makes about the simplest possible plain text template and
;; returns the result of calling it
(defun easy-template-example ()
  (let ((fn
	 (talcl:compile-tal-string
	  "<tal:tal
            xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
            >Super Simple Template</tal:tal>")))
    (talcl:run-template-fn fn (talcl:tal-env))))

;; Makes a very simple plain text template that splices a single
;; value into your simple template
(defun easy-template-example-2 ()
  (let ((fn
	 (talcl:compile-tal-string
	  "<tal:tal tal:in-package=\"talcl\"
            xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
            >Super Simple Template $this-is-some-data </tal:tal>")))
    (list
     ;; the package of the symbols in the env, needs to match their package
     ;; when the template is evaluated, see tal:in-package
     (talcl:run-template-fn
      fn (talcl:tal-env 'this-is-some-data "My Data"))
     (talcl:run-template-fn
      fn (talcl:tal-env 'this-is-some-data "Your Data")))))

;; Generators help locate templates files on disk, this one
;; will locate files for the system in the "examples" dir

(defvar *example-generator*
  (make-instance 'talcl:caching-file-system-generator
		 :root-directories (list (asdf:system-relative-pathname
					  :talcl #p"examples/"))))

;; an example of building a dom snippet, to be spliced into the template
(defun example-footer-menu ()
  (if buildnode:*document*
      (xhtml:ul ()
	(iter (for i from 0 to 4)
	      (for name = (format nil "~D footer list item" i))
	      (collect (xhtml:li `(:title ,name) name ))))
      "<ul><li>0 footer list item</li><li>1 footer list item</li></ul>")
  )

(defun process-example-window-to-string ()
  (let ((net.acceleration.buildnode:*html-compatibility-mode* t)
	(fn (talcl:load-tal *example-generator* "window.tal")))
    (talcl:run-template-fn
     fn (talcl:tal-env 'page-title "My Example Page"
		       'header-dom-id "header"
		       'body "<div>Some Body Content</div>"
		       'escaped-html-string "<span >Escaped HTML</span>"
		       'unescaped-html-string "<span >Unescaped HTML</span>"
		       ))))



;; This will put a template into a dom document, then render out that document
(defun process-example-window-to-string-using-dom-document ()
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