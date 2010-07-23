;; Copyright (c) 2003, Miles Egan
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;
;;     * The name of the author may not be used to endorse or promote
;;       products derived from this software without specific prior
;;       written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; $Id: xmls.lisp 447 2003-08-29 04:40:29Z miles $

;;;; * Miles Egan's XMLS

(defpackage :it.bese.yaclml.xmls
  (:use :cl)
  (:export #:node-name
           #:node-ns
           #:node-attrs
           #:node-children
           #:make-node
           #:parse
           #:*entities*
           #:*convert-entities*))

(in-package :it.bese.yaclml.xmls)

;;;-----------------------------------------------------------------------------
;;; GLOBAL SETTINGS
;;;-----------------------------------------------------------------------------
(defvar *strip-comments* t)
(defvar *test-verbose* nil)

(defvar *entities*
  '(("lt;" #\<)
    ("gt;" #\>)
    ("amp;" #\&)
    ("apos;" #\')
    ("quot;" #\")
    ("nbsp;" #\Space)))

(defvar *convert-entities* t
  "When true we convert entities found in the data to their
  corresponding chars, when false we leave ignore entities. NB:
  in the current implementation we are only able to convert a
  limited subset of all entities (see *entities* for the complete
  listing).")

;;;-----------------------------------------------------------------------------
;;; CONDITIONS
;;;-----------------------------------------------------------------------------
(define-condition xml-parse-error (error) 
  ((offset :accessor offset :initform nil :initarg :offset)
   (message :accessor message :initform nil :initarg :message))
  (:report (lambda (c s)
             (if (message c)
                 (format s (message c))
                 (format s "XML-PARSE-ERROR at offset ~D." (offset c))))))

(define-condition unresovable-entity (xml-parse-error)
  ((entity :initarg :entity :accessor entity))
  (:report (lambda (c s)
             (format s "Unable to resolve entity ~S at offset ~D." (entity c) (offset c)))))

(define-condition reference-to-undeclared-namespace (xml-parse-error)
  ((namespace :accessor namespace :initarg :namespace))
  (:report (lambda (c s)
             (format s  "Undeclared namespace ~S referenced." (namespace c)))))

(define-condition unmatched-end-tag (xml-parse-error)
  ((expected :accessor expected :initarg :expected)
   (found :accessor found :initarg :found))
  (:report (lambda (c s)
             (format s "Unmatched end tag, found ~S and was expecting ~S." (found c) (expected c)))))

;;;-----------------------------------------------------------------------------
;;; NODE INTERFACE
;;;-----------------------------------------------------------------------------
(defun make-node (&key name ns attrs child children)
  "Convenience function for creating a new xml node."
  (cons (cons (if ns 
                  (cons name ns)
                  name)
              attrs)
        (if child
            (list child)
            children)))

(defun node-name (elem)
  (if (consp (caar elem))
      (caaar elem)
      (caar elem)))

(defun (setf node-name) (name elem)
  (setf (caar elem) name))

(defun node-ns (elem)
  (if (consp (caar elem))
      (cdaar elem)
      nil))

(defun (setf node-ns) (ns elem)
  (if (consp (caar elem))
      (setf (cdaar elem) ns)
      (progn
        (setf (caar elem) (cons (caar elem) ns))
        ns)))

(defun node-attrs (elem) 
  (cdar elem))

(defun (setf node-attrs) (attrs elem) 
  (setf (cdar elem) attrs))

(defun node-children (elem)
  (cddr elem))

(defun (setf node-children) (children elem) 
  (setf (cdr elem) children)
  (node-children elem))

;;;-----------------------------------------------------------------------------
;;; UTILITY FUNCTIONS
;;;-----------------------------------------------------------------------------
  
(defun entity-of (char)
  "Returns the xml entity corresponding to CHAR, without the leading
ampersand. Returns NIL if not found."
  (declare (type list *entities*))
  (first (find char *entities* 
	       :test #'char= 
	       :key (lambda (e) (second e)))))

(defun entitify (object)
  "Converts OBJECT to its string representation, if necessary, and then replaces 
the characters of OBJECT with their corresponding entities. Assumes that the 
characters of RESERVED have been registered in the entity table."
  (let ((str (typecase object
               (string (coerce object 'simple-base-string))
               (t (format nil "~A" object)))))
    (declare (type simple-base-string str)) ;; mollify sbcl compiler
    (loop with stream = (make-string-output-stream)
          for c across str
          for ent = (entity-of c)
          for code = (char-code c)
          do (cond
               (ent
                (progn
                  (write-char #\& stream)
                  (write-string ent stream)))
               ((and (or (< code 32) (> code 126))
                     (not (= code 10))
                     (not (= code 9)))
                (format stream "&#x~x;" code))
               (t
                (write-char c stream)))
          finally (return (get-output-stream-string stream)))))

(defun make-extendable-string (&optional (size 10))
  "Creates an adjustable string with a fill pointer."
  (make-array size
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defun push-string (c string)
  "Shorthand function for adding characters to an extendable string."
  (vector-push-extend c string))

;;;-----------------------------------------------------------------------------
;;; PARSER STATE & LOOKAHEAD
;;;-----------------------------------------------------------------------------
(defstruct state
  "Represents parser state.  Passed among rules to avoid threading issues."
  (got-doctype nil)
  (lines 1 :type integer)
  nsstack
  stream)

(defun resolve-entity (ent)
  "Resolves the xml entity ENT to a character.  Numeric entities are
converted using CODE-CHAR, which only works in implementations that
internally encode strings in US-ASCII, ISO-8859-1 or UCS."
  #-sb-unicode (declare (type simple-base-string ent))
  #+sb-unicode (declare (type string ent))
  (or (and (>= (length ent) 2)
           (char= (char ent 0) #\#)
           (code-char
            (if (char= (char ent 1) #\x)
                (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
                (parse-integer ent :start 1 :end (- (length ent) 1)))))
      (second (assoc ent *entities* :test #'string=))
      (error 'unresovable-entity :entity ent)))

(declaim (inline peek-stream))
(defun peek-stream (stream)
  "Looks one character ahead in the input stream.  Serves as a potential hook for
character translation."
  (peek-char nil stream nil))

(defun read-stream (stream)
  "Reads a character from the stream, translating entities as it
goes (assuming *convert-entities* is non-NIL)."
  (let ((c (read-char stream nil)))
    (if (or (not (char= c #\&))
            (not *convert-entities*))
        c
        (loop with ent = (make-extendable-string 5)
           for char = (read-char stream)
           do (push-string char ent)
           until (char= char #\;)
           finally (return (resolve-entity (coerce ent 'simple-string)))))))

(define-symbol-macro next-char (peek-stream (state-stream s)))

(defmacro eat ()
  "Consumes one character from the input stream."
  `(read-char (state-stream s)))

(defmacro match (&rest matchers)
  "Attempts to match the next input character with one of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (and
     (or ,@(loop for m in matchers
                 collect (etypecase m
                           (standard-char `(char= ,m c))
                           (symbol `(,m c)))))
     ;; cheat here a little bit - eat entire char entity instead
     ;; of peeked char
     (read-stream (state-stream s)))))

(defmacro match-seq (&rest sequence)
  "Tries to match the supplied matchers in sequence with characters in the input stream."
  `(and ,@(loop for s in sequence
                collect `(match ,s))))

(defmacro match* (&rest sequence)
  "Matches any occurances of any of the supplied matchers."
  `(loop with data = (make-extendable-string 10)
    for c = (match ,@sequence)
    while c
    do (push-string c data)
    finally (return data)))

(defmacro match+ (&rest sequence)
  "Matches one or more occurances of any of the supplied matchers."
  `(and (peek ,@sequence)
    (match* ,@sequence)))

(defmacro peek (&rest matchers)
  "Looks ahead for an occurance of any of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (or ,@(loop for m in matchers
                collect (etypecase m
                          (standard-char `(char= ,m c))
                          (symbol `(,m c)))))))

(defmacro must ((error-type &rest error-args) &rest body)
  "Throws a parse error if the supplied forms do not succeed."
  `(or (progn ,@body)
       (error ',error-type ,@error-args)))

;;;-----------------------------------------------------------------------------
;;; PARSER INTERNAL FUNCTIONS
;;;-----------------------------------------------------------------------------
(defstruct element
  "Common return type of all rule functions."
  (type nil :type symbol)
  (val nil))

(defun lookup-namespace (ns env)
  (dolist (e env)
    (let ((nsurl (assoc ns e :test #'string=)))
      (when nsurl
        (return-from lookup-namespace (cdr nsurl))))))

(defun lookup-package (ns env)
  (declare (special uri-to-package))
  (if ns
      (or (cdr (assoc (or (lookup-namespace ns env)
                          (error 'reference-to-undeclared-namespace :namespace ns))
                      uri-to-package :test #'string=))
          (error 'reference-to-undeclared-namespace :namespace ns))
      *package*))

(defun intern-xml-name (name ns env)
  (intern (ecase (readtable-case *readtable*)
            (:upcase (string-upcase name))
            (:downcase (string-downcase name))
            (:preserve name)
            (:invert (if (every #'upper-case-p name)
                         (string-downcase name)
                         (if (every #'lower-case-p name)
                             (string-upcase name)
                             name))))
          (lookup-package ns env)))

(defun resolve-namespace (elem env)
  "Maps the ns prefix to its associated url via the supplied ns env."
  (setf (node-name elem) (intern-xml-name (node-name elem) (node-ns elem) env))
  (loop
     for attr on (node-attrs elem) by #'cddr
     do (setf (car attr) (intern-xml-name (caar attr) (cdar attr) env)))
  t)

;;;-----------------------------------------------------------------------------
;;; MATCH AND RULE BUILDING UTILITIES
;;;-----------------------------------------------------------------------------
(defmacro defmatch (name &rest body)
  "Match definition macro that provides a common lexical environment for matchers."
  `(defun ,name (c)
    ,@body))
  
(defmacro defrule (name &rest body)
  "Rule definition macro that provides a common lexical environment for rules."
  `(defun ,name (s)
    ,@body))

(defmacro matchfn (name)
  "Convenience macro for creating an anonymous function wrapper around a matcher macro."
  `(lambda (s) (match ,name)))

(defun none-or-more (s func)
  "Collects any matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (values res (cons val nextval)))
        (values t nil))))

(defun one-or-more (s func)
  "Collects one or more matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (declare (ignore res))
          (cons val nextval))
        nil)))

;;;-----------------------------------------------------------------------------
;;; MATCHERS
;;;-----------------------------------------------------------------------------
(defmatch digit ()
  (and c (digit-char-p c)))

(defmatch letter ()
  (and c (alpha-char-p c)))

(defmatch ws-char ()
  (case c
    ((#\Newline #\Space #\Tab #\Return . #.(unless (char= #\Newline #\Linefeed) (list #\Linefeed))) t)
    (t nil)))

(defmatch namechar ()
  (or 
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_ #\:) t))))

(defmatch ncname-char ()
  (or 
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_) t))))

(defmatch attr-text-dq ()
  (and c
       (case c
         ((#\< #\") nil)
         (t t))))

(defmatch attr-text-sq ()
  (and c
       (case c
         ((#\< #\') nil)
         (t t))))

(defmatch chardata ()
  (and c (not (char= c #\<))))

(defmatch comment-char ()
  (and c (not (eql c #\-))))

;;;-----------------------------------------------------------------------------
;;; RULES
;;;-----------------------------------------------------------------------------
(defrule ncname ()
  (and (peek letter #\_)
       (match+ ncname-char)))

(defrule qname ()
  (let (name suffix)
    (and
     (setf name (ncname s))
     (or
      (and
       (match #\:)
       (setf suffix (ncname s)))
      t))
    (values name suffix)))

(defrule attr-or-nsdecl ()
  (let (suffix name val)
    (and
     (setf (values name suffix) (qname s))
     (or
      (and
       (match #\=)
       (or
        (and
         (match #\")
         (setf val (match* attr-text-dq))
         (match #\"))
        (and
         (match #\')
         (setf val (match* attr-text-sq))
         (match #\'))))
      t)
     (if (string= "xmlns" name)
         (cons 'nsdecl (cons suffix val)) 
         (list
          'attr 
          (if suffix
              (cons suffix name)
              (cons name nil))
          val)))))

(defrule ws ()
  (and (match+ ws-char)
       (make-element :type 'whitespace :val nil)))

(defrule name ()
  (and
   (peek namechar #\_ #\:)
   (match* namechar)))

(defrule ws-attr-or-nsdecl ()
  (and
   (ws s)
   (attr-or-nsdecl s)))

(defrule start-tag ()
  (let (name suffix attrs nsdecls)
    (and
     (peek namechar)
     (setf (values name suffix) (qname s))
     (multiple-value-bind (res a)
         (none-or-more s #'ws-attr-or-nsdecl)
       (mapcar (lambda (x) (if (eq (car x) 'attr)
                               (setf attrs (nconc (cdr x) attrs))
                               (push (cdr x) nsdecls)))
               a)
       res)
     (or (ws s) t)
     (values
      (make-node
       :name (or suffix name)
       :ns (and suffix name)
       :attrs attrs)
      nsdecls))))

(defrule end-tag ()
  (let (name suffix)
    (and
     (match #\/)
     (setf (values name suffix) (qname s))
     (or (ws s) t)
     (match #\>)
     (make-element :type 'end-tag :val (intern-xml-name (if suffix
                                                            suffix
                                                            name)
                                                        (if suffix
                                                            name
                                                            nil)
                                                        (state-nsstack s))))))

(defrule comment ()
  (and
   (match-seq #\! #\- #\-)
   (progn
     (loop until (match-seq #\- #\- #\>)
           do (eat))
     t)
   (make-element :type 'comment)))

(defrule comment-or-cdata ()
  (and
   (peek #\!)
   (must (xml-parse-error :offset (file-position (state-stream s)))
         (or (comment s)
             (and
              (match-seq #\[ #\C #\D #\A #\T #\A #\[)
              (loop with data = (make-extendable-string 50)
                 with state = 0
                 do (case state
                      (0 (if (match #\])
                             (incf state)
                             (push-string (eat) data)))
                      (1 (if (match #\])
                             (incf state)
                             (progn 
                               (setf state 0)
                               (push-string #\] data)
                               (push-string (eat) data))))
                      (2 (if (match #\>)
                             (incf state)
                             (progn 
                               (setf state 0)
                               (push-string #\] data)
                               (push-string #\] data)
                               (push-string (eat) data)))))
                 until (eq state 3)
                 finally (return (make-element :type 'cdata :val data))))))))

(declaim (ftype function element))     ; forward decl for content rule
(defrule content ()
  (if (match #\<)
      (must (xml-parse-error :offset (file-position (state-stream s)))
            (or (comment-or-cdata s)
                (element s)
                (end-tag s)))
      (or (let (content)
            (and (setf content (match+ chardata))
                 (make-element :type 'data :val content))))))

(defrule element ()
  (let (elem children nsdecls end-name)
    (and
     ;; parse front end of tag
     (multiple-value-bind (e n)
         (start-tag s)
       (setf elem e)
       (setf nsdecls n)
       e)
     ;; resolve namespaces *before* parsing children
     (if nsdecls
         (push nsdecls (state-nsstack s))
         t)
     (resolve-namespace elem (state-nsstack s))
     ;; parse end-tag and children
     (or
      (match-seq #\/ #\>)
      (and
       (match #\>)
       (loop for c = (content s)
             while c
             do (etypecase c
                  (element (case (element-type c)
                             ('end-tag
                              (return (setf end-name (element-val c))))
                             (t (if (element-val c)
                                    (push (element-val c) children)))))))
       (or (eql (node-name elem) end-name)
           (error 'unmatched-end-tag :found end-name :expected (node-name elem)))))
     ;; package up new node
     (progn
       (setf (node-children elem) (nreverse children))
       (make-element :type 'elem :val elem)))))

(defrule processing-instruction-or-xmldecl ()
  (let (name)
    (and
     (match #\?)
     (setf name (name s))
     (none-or-more s #'ws-attr-or-nsdecl)
     (match-seq #\? #\>)
     (make-element :type 'pi :val name))))

(defrule processing-instruction ()
  (let ((p (processing-instruction-or-xmldecl s)))
    (and p
         (not (string= (element-val p) "xml"))
         p)))

(defrule xmldecl ()
  (let ((p (processing-instruction-or-xmldecl s)))
    (and p
         (string= (element-val p) "xml")
         p)))

(defrule comment-or-doctype ()
  ;; skip dtd - bail out to comment if it's a comment
  ;; only match doctype once
  (and
   (peek #\!)
   (or (comment s)
       (and (not (state-got-doctype s))
            (must (xml-parse-error :offset (file-position (state-stream s)))
                  (match-seq #\D #\O #\C #\T #\Y #\P #\E))
            (loop with level = 1
                  do (case (eat)
                       (#\> (decf level))
                       (#\< (incf level)))
                  until (eq level 0)
                  finally (return t))
            (setf (state-got-doctype s) t)
            (make-element :type 'doctype)))))

(defrule misc ()
  (or 
   (ws s)
   (and (match #\<) (must (xml-parse-error :offset (file-position (state-stream s)))
                          (or (processing-instruction s)
                              (comment-or-doctype s)
                              (element s))))))

(defrule document ()
  (let (elem)
    (if (match #\<)
        (must (xml-parse-error :offset (file-position (state-stream s)))
              (or (processing-instruction-or-xmldecl s)
                  (comment-or-doctype s)
                  (setf elem (element s)))))
    (unless elem
      (loop for c = (misc s)
            while c do (if (eql (element-type c) 'elem)
                           (return (setf elem c)))))
    (and elem (element-val elem))))

;;;-----------------------------------------------------------------------------
;;; PUBLIC INTERFACE
;;;-----------------------------------------------------------------------------
(defun parse (s &key uri-to-package)
  "Parses the supplied stream or string into a lisp node tree."
  (declare (special uri-to-package))
  (let ((*uri-to-package* uri-to-package)
	(stream (etypecase s
		  (string (make-string-input-stream s))
		  (stream s))))
    (declare (special *uri-to-package*))
    (handler-bind ((xml-parse-error (lambda (c)
                                      (unless (offset c)
                                        (setf (offset c) (file-position stream))))))
      (document (make-state :stream stream)))))
