(defsystem :talcl
  :description "TAL in CL"
  :licence "BSD (sans advertising clause)"
  :version "0.2"
  :components
  ((:module :src
	    :serial T
	    :components ((:file "packages")
			 (:file "tal-environment")
			 (:file "sax-buffer")
			 (:file "compile")
			 (:file "dom")
			 (:file "generator")
			 (:file "handlers"))
	    ))
  :depends-on (:buildnode :cxml :iterate :arnesi :adwcodebase))

(defsystem :talcl-test
  :description "talcl-test tests for talcl tal templating library"
  :author "Acceleration.net"
  :licence "BSD (sans advertising clause)"
  :version "0.2"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "compile"))))
  :depends-on (:talcl :lisp-unit))