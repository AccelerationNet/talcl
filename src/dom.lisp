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
      (if (typep data 'buffering-sink)
	  (stop-buffering-and-flush data sink)
	  (error "Malformed processing-instruction: if target (eql :tal) then data should be a function of (sink)."))
      (call-next-method)))

(defun tal-processing-instruction (generator template-name env)
  "Make a tal-processing-instruction. It's a dom node that when processed
will insert the template in the sax stream."
  (make-instance
   'template-node
   :owner buildnode:*document*
   :target :tal
   :data (buffered-template-call (load-tal generator template-name) env)
   ))


(defun dom-walk-helper (tree)
  (cxml:with-output-sink (cxml::*sink*)
    (loop for n in (ensure-list tree)
	  do (typecase n
	       (string (cxml:text n))
	       (list (dom-walk-helper n))
	       (dom:node (dom:walk cxml::*sink* n))))))

