(defsystem :talcl
  :description "TAL in CL"
  :licence "BSD (sans advertising clause)"
  :version "0.2"
  :components
  ((:module :src
	    :components ((:file "packages")
			 (:file "tal-environment" :depends-on ("packages"))
			 (:file "compile" :depends-on ("packages" "tal-environment"))
			 (:file "generator" :depends-on ("packages" "compile"))
			 (:file "handlers" :depends-on ("packages" "compile")
				:depends-on ("packages" "compile" "dom"))
			 (:file "dom" :depends-on ("packages" "compile")))
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