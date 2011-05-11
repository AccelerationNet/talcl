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
will insert the template in the sax stream.

   The template content will never be part of the dom, which saves two changes
   of representation. This is about 3 times faster than tal-template-content.
   "
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
	       (dom:node (%dom-walk cxml::*sink* n))))))

(defun make-output-sink (stream &key canonical indentation (char-p T))
  (make-template-processing-sink
   (buildnode:make-output-sink
    stream :canonical canonical :indentation indentation :char-p char-p)))

(defun tal-template-content (generator template-name env &optional (node buildnode:*document*))
  "Serialize a template into the document as children of the node passed in

   This is about 3 times slower than the tal-processing-instruction
  "
  (let* ((sink (make-template-processing-sink
		(buildnode:make-scoped-dom-builder node))))
    ;(setf (cxml::sink-omit-xml-declaration-p cxml::*sink*) T)
    (with-this-sink (sink)      
      (talcl::%call-template-with-tal-environment
       (load-tal generator template-name)
       env))
    ;; documents cannot be appended to a document
    ;; but we wish to support
    ;; (with-html-document (tal-template-content ... (xhtml:div ())))
    (etypecase node
      (dom:element node)
      (dom:document (values)))))

(defun document-to-stream ( doc stream )
  (let* ((buildnode::*html-compatibility-mode* (buildnode::html-output? doc))
	 (sink (talcl::make-output-sink stream)))
    (buildnode::write-normalized-document-to-sink doc sink)))

(defun document-to-string ( doc )
  (with-output-to-string (s)
    (talcl::document-to-stream doc s)))


