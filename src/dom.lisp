(in-package :talcl)

(defclass template-processing-sink (cxml:broadcast-handler) ())

(defun make-template-processing-sink (&rest handlers)
  (make-instance 'template-processing-sink :handlers handlers))


(defclass template-node (rune-dom::processing-instruction)
  ((dom:node-name :accessor dom:node-name :initarg :node-name :initform "template-node")))

(defmethod sax:processing-instruction ((sink template-processing-sink)
				       target
				       data)
  ;;We're assuming the processing instruction has been created
  ;;with target = :tal and data being the function to call.
  (if (or (eql target :tal)
	  (equal target "tal"))
      (if (functionp data)
	  (funcall data sink)
	  (error "Malformed processing-instruction: if target (eql :tal) then data should be a function of (sink)."))
      (call-next-method)))


(defvar *template-sink* nil)



(defun tal-processing-instruction (generator template-name env)
  "Make a tal-processing-instruction. It's a dom node that when processed
will insert the template in the sax stream."
  (flet ((template-node-fn (sink)
	   ;;(cxml::maybe-close-tag sink)
	   (let ((cxml::*sink* sink)
		 (cxml::*current-element* nil)
		 (cxml::*unparse-namespace-bindings* cxml::*initial-namespace-bindings*)
		 (cxml::*current-namespace-bindings* nil))
	     (let ((buildnode:*document* cxml::*walk-document*) ; do we actually need this?
		   )
	       (funcall (load-tal generator template-name) env)))))
    (let ((tn
    (make-instance 'template-node
		   :owner buildnode:*document*
		   :target :tal
		   :data #'template-node-fn
		   )))
      
      tn)))


(defun dom-walk-helper (tree)
  (cxml:with-output-sink (*template-sink*)
    (loop for n in (arnesi:ensure-list tree)
	  do (typecase n
	       (string (cxml:text n))
	       (list (dom-walk-helper n))
	       (dom:node (dom:walk *template-sink* n))))))

(def-attribute-handler tal::dom-content (tag)
  "Becomes a TAL:REPLACE."
  (let ((value (pull-attrib-val! tag 'tal::dom-content)))
    (destructuring-bind (tag-name attributes &rest body) tag
      (declare (ignore body))
      (talcl:transform-lxml-form
       `(,tag-name ,attributes
		   (CONTENT-DUMMY ((tal::replace ,value))
				  "DUMMY"))))))
