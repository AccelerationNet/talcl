(in-package :talcl)

(defclass extended-xmls-builder (cxml-xmls::xmls-builder)
  ()
  (:documentation "an xmls builder that includes comments as (:comment () comment-text) nodes"))

(defun make-extended-xmls-builder (&key (include-default-values t)
				   (include-namespace-uri t))
  "Make a XMLS style builder.  When 'include-namespace-uri is true a modified
  XMLS tree is generated that includes the element namespace URI rather than
  the qualified name prefix and also includes the namespace URI for attributes."
  (make-instance 'extended-xmls-builder
		 :include-default-values include-default-values
		 :include-namespace-uri include-namespace-uri))

(defmethod sax:comment ((handler extended-xmls-builder) data)
  (let* ((parent (car (cxml-xmls::element-stack handler)))
	 (node (list :comment () data)))
    (push node (cxml-xmls::node-children parent))))

(defclass buffering-sink (cxml:broadcast-handler)
  ((buffering :accessor buffering :initarg :buffering :initform nil
	      :documentation "Is this currently buffering")
   (flushing :accessor flushing :initarg :flushing :initform nil
	     :documentation "Is this currently flushing")
   (buffer :accessor buffer :initarg :buffer :initform nil
	   :documentation "The events buffered so far")))

(defgeneric start-buffering (sink)
  (:method ((o buffering-sink))
    (setf (buffering o) T)))

(defgeneric stop-buffering-and-flush (buffer &optional output-sink)
  (:method ((o buffering-sink) &optional output-sink)
    (setf (flushing o) T
          (buffering o) nil)
    (iter (for (fn . args) in (reverse (buffer o)))
      ;; if we have
      (apply fn (or output-sink o) args))
    ;; Set buffering to nil after flusing, so that we can use
    ;; that flag to not double buffer
    (setf (flushing o) nil)))

(defgeneric clear-buffer (sink)
  (:method ((o buffering-sink))
    (setf (buffer o) nil)))

(macrolet ((define-proxy-method (name (&rest args))
	       `(defmethod ,name ((handler buffering-sink) ,@args)
		  (if (and (buffering handler)
			   (not (flushing handler)))
		      (push (list #',name ,@args) (buffer handler))
		      (call-next-method)))))
  (define-proxy-method sax:start-document ())
  (define-proxy-method sax:start-element (uri lname qname attributes))
  (define-proxy-method sax:start-prefix-mapping (prefix uri))
  (define-proxy-method sax:characters (data))
  (define-proxy-method sax:unescaped (data))
  (define-proxy-method sax:processing-instruction (target data))
  (define-proxy-method sax:end-prefix-mapping (prefix))
  (define-proxy-method sax:end-element (namespace-uri local-name qname))
  (define-proxy-method sax:end-document ())
  (define-proxy-method sax:comment (data))
  (define-proxy-method sax:start-cdata ())
  (define-proxy-method sax:end-cdata ())
  (define-proxy-method sax:start-dtd (name public-id system-id))
  (define-proxy-method sax:end-dtd ())
  (define-proxy-method sax:start-internal-subset ())
  (define-proxy-method sax:end-internal-subset ())
  (define-proxy-method sax:unparsed-entity-declaration (name pub sys not))
  (define-proxy-method sax:external-entity-declaration (kind name pub sys))
  (define-proxy-method sax:internal-entity-declaration (kind name value))
  (define-proxy-method sax:notation-declaration (name public-id system-id))
  (define-proxy-method sax:element-declaration (name model))
  (define-proxy-method sax:attribute-declaration (elt attr type default))
  (define-proxy-method sax:entity-resolver (resolver))
  (define-proxy-method sax::dtd (dtd)))