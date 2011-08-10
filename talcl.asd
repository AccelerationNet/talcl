;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :talcl.system)
    (defpackage :talcl.system
	(:use :common-lisp :asdf))))

(in-package :talcl.system)

(defsystem :talcl
  :description "TAL in CL"
  :licence "BSD"
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
  :depends-on (:buildnode :cxml :iterate :alexandria))

(defsystem :talcl-examples
  :description "talcl-examples examples for talcl tal templating library"
  :author "Acceleration.net"
  :licence "BSD"
  :version "0.2"
  :components
  ((:module :examples
	    :serial t
	    :components ((:file "tal-examples"))))
  :depends-on (:talcl :buildnode-xhtml))

(defsystem :talcl-test
  :description "talcl-test tests for talcl tal templating library"
  :author "Acceleration.net"
  :licence "BSD"
  :version "0.2"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "compile"))))
  :depends-on (:talcl :lisp-unit :buildnode-xhtml))

(defsystem :talcl-speed-tests
  :description "talcl-test tests for talcl tal templating library"
  :author "Acceleration.net"
  :licence "BSD"
  :version "0.2"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "speed"))))
  :depends-on (:talcl :lisp-unit :buildnode-xhtml :talcl-examples))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :talcl))))
  (asdf:oos 'asdf:load-op :talcl-test)
  (funcall (intern "RUN-TESTS" :talcl-test)
	   :use-debugger nil))

;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;;;   email: bobbysmith007@gmail.com
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.
