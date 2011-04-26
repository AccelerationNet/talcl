(defsystem :talcl
  :description "TAL in CL"
  :licence "LGPL"
  :version "0.2"
  :components
  ((:module :src
	    :serial T
	    :components ((:file "packages")
			 (:file "tal-environment")
			 ;; should be removed once a patch doing the same thing makes its
			 ;; way upstream
			 (:file "dom-walker")
			 (:file "sax-buffer")
			 (:file "compile")
			 (:file "dom")
			 (:file "generator")
			 (:file "handlers"))
	    ))
  :depends-on (:buildnode :cxml :iterate))

(defsystem :talcl-test
  :description "talcl-test tests for talcl tal templating library"
  :author "Acceleration.net"
  :licence "LGPL"
  :version "0.2"
  :components
  ((:module :tests
	    :serial t
	    :components ((:file "setup")
			 (:file "compile"))))
  :depends-on (:talcl :lisp-unit))

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
