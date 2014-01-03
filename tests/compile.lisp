(in-package :talcl-test)

(adwtest test-missing-variable-declarations (compile-tests missing-values)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\" id=\"${a}\">${b}<span>${c}</span></div>"
            )
        (fn (talcl::compile-tal-string-to-lambda
             it :declare-unbound-variables-special? nil)))
    (assert-equal '(a b c) (find-missing-template-variables fn))
    (assert-equal '(a b c) (find-missing-template-variables-from-unbound-variable-errors fn))
    #+sbcl
    (assert-equal '(a b c) (find-missing-template-variables-from-warnings fn))
    
    ))

(adwtest test-comment (compile-tests)
  (let* ((comment "<!-- Awesome Comment -->")
	 (it (format
	      nil
	      "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">A Comment:~AEND</div>"
	      comment))
	 (fn (talcl::compile-tal-string it))
	 (out (buffer-xml-output ()
		(talcl::%call-template-with-tal-environment fn '()))))
    (assert-true
     (search comment out :test #'char=)
     out)
    ))

(adwtest test-whitespace (compile-tests whitespace-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">$test should have whitespace after</div>")
	 (fn (talcl::compile-tal-string it))
	 (out (buffer-xml-output ()
		(talcl::%call-template-with-tal-environment fn '(test "This test")))))
    (assert-true
     (search "This test should have whitespace after" out :test #'char=)
     out)
    ))

(adwtest test-whitespace2 (compile-tests whitespace-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">This ${test} should have whitespace after</div>")
	 (fn (talcl::compile-tal-string it))
	 (out (buffer-xml-output ()
		(talcl::%call-template-with-tal-environment fn '(test "new test")))))
    (assert-true
     (search "This new test should have whitespace after" out :test #'char=)
     out)
    ))

(adwtest test-missing-value (runtime-tests missing-values)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">Test that $test should be missing</div>")
	 (fn (talcl::compile-tal-string it)))

    (assert-error
     'talcl::tal-runtime-condition
     (buffer-xml-output ()
       (talcl::%call-template-with-tal-environment fn '())))))

(adwtest test-missing-value2 (runtime-tests missing-values)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">Test that $test should be missing</div>")
	 (fn (talcl::compile-tal-string it)))

    (let* ((talcl::*default-missing-template-value* "<MISSING>")
	   (out (buffer-xml-output ()
		  (talcl::%call-template-with-tal-environment fn '()))))
      (assert-true
       (search "Test that &lt;MISSING&gt; should be missing" out :test #'char=)
       out))))

(adwtest test-missing-value3 (runtime-tests missing-values)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">Test that $test should be missing</div>")
	 (fn (talcl::compile-tal-string it)))

    (let* ((out (with-missing-value-handler
		    ((name)
		     (assert-equal 'test name)
		     "<MISSING>")
		  (buffer-xml-output ()
		    (talcl::%call-template-with-tal-environment fn '()))
		  )))
      (assert-true
       (search "Test that &lt;MISSING&gt; should be missing" out :test #'char=)
       out))))

(adwtest test-in-package (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
               ${ (assert-equal (find-package :talcl-test) *expression-package*) }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-let (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
                  <span tal:let=\"x 3\">
                   ${ (assert-equal x 3) }
                  </span>
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-attrib-value-quoting (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
                  <span foo=\"$foo-val\">
                  </span>
          </div>")
	 (fn (talcl::compile-tal-string it))
	 (res  (run-template-fn  fn `(foo-val "\"quoted\""))))
    (assert-true
     (search "&quot;quoted&quot;" res :test #'string-equal)
     res)))

(adwtest test-left-to-right-attrib-eval (compile-tests)
  " Verify that the in-package takes place before the let,
    so that vars are interned in the correct place
  "
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               tal:let=\"lst (list 1 2 3 4 5) test 0\">
                  ${ (assert-equal (length lst) 5) }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-loop (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               tal:let=\"lst (list 1 2 3 4 5) test 0\">
                  ${ (assert-equal (length lst) 5) }
                  <tal:loop tal:var=\"val\" tal:list=\"lst\" >
                   ${ (assert-equal val (incf test)) }
                  </tal:loop>
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-constant-loop (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               tal:let=\"test 0\">
                  <tal:loop tal:var=\"val\" tal:idx=\"i\" tal:constant-list=\"1,2,3,4,5\" >
                   ${ (assert-equal test i) }
                   ${ (assert-equalp val (princ-to-string (incf test))) }
                  </tal:loop>
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-test-generator (compile-meta-tests compile-test)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
               ${ (assert-true T \"This is just so we can be sure this ran\") }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (add-tal *test-generator* "test" fn)
    (talcl:call-template-with-tal-environment *test-generator* "test" ())))

(defvar *test-count* nil)

(adwtest test-basic-include (compile-tests)
  (setf *test-count* 0)
  (add-tal *test-generator* "basic"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                This is included content ${ (incf *test-count*) }</div>")
  (add-tal *test-generator* "test"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 tal:in-package=\"talcl-test\">
               <tal:include tal:name=\"basic\" />
               <tal:include tal:name=\"basic\" />
               <tal:include tal:name=\"basic\" />
          </div>")
    (talcl:call-template-with-tal-environment *test-generator* "test" ())
    (assert-equal *test-count* 3
     "We included 3 times so we should have incf'ed 3 times"))

(defun %test-include-name (cnt name)
  (string-equal name (case cnt (1 "FIRST") (2 "SECOND") (3 "THIRD"))))

(adwtest test-include-param-attribs (compile-tests)  
    (setf *test-count* 0)
    (add-tal *test-generator* "basic"
	     "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                This is included content ${ (incf *test-count*) }, param:${value}
            ${ (assert-true (%test-include-name *test-count* value)) }
            </div>")
    (add-tal *test-generator* "test"
	     "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 xmlns:param=\"http://common-lisp.net/project/bese/tal/params\"
                 tal:in-package=\"talcl-test\"
                 tal:let=\"pname2 'second pname3 'third\">
               <tal:include tal:name=\"basic\" param:value=\"FIRST\" />
               <tal:include tal:name=\"basic\" param:value=\"${pname2}\" />
               <tal:include tal:name=\"basic\" param:value=\"${pname3}\" />
          </div>")
    (talcl:call-template-with-tal-environment
     *test-generator* "test" ())
    (assert-equal *test-count* 3
		  "We included 3 times so we should have incf'ed 3 times"))

(defun %test-include-body-name (cnt name)
  (string-equal name (case cnt (1 "FIRST") (2 "SECOND") (3 "<div>THIRD</div>"))))

(adwtest test-include-param-body (compile-tests)
  (setf *test-count* 0)
  (add-tal *test-generator* "basic"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                This is included content ${ (incf *test-count*) }, param:${value}
                <span class=\"content\" tal:content=\"value\" >${ (assert-true nil) }</span>
                <tal:tal tal:replace=\"value\" />
                ${ (assert-typep 'buffering-sink value )
                   (assert-true (and
                                 (typep value 'buffering-sink )
                                 (%test-include-body-name
                                 *test-count*
                                 (talcl::buffer-xml-output ()
                                    (stop-buffering-and-flush value cxml::*sink*)))))
                 }
            </div>")
  (add-tal *test-generator* "test"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 xmlns:param=\"http://common-lisp.net/project/bese/tal/params\"
                 tal:in-package=\"talcl-test\"
                 tal:let=\"pname2 'second pname3 'third\">
               <tal:include tal:name=\"basic\" ><param:value>FIRST</param:value></tal:include>
               <tal:include tal:name=\"basic\" >
                 <param:value>${pname2}</param:value></tal:include>
               <tal:include tal:name=\"basic\" >
                 <param:value><div>${pname3}</div></param:value></tal:include>
           </div>")
  (talcl:call-template-with-tal-environment *test-generator* "test" ())
  (assert-equal *test-count* 3
		"We included 3 times so we should have incf'ed 3 times"))

(adwtest test-include-param-body-as-string (compile-tests)
  (setf *test-count* 0)
  (add-tal *test-generator* "basic"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                 $(assert-true (stringp value))
                 $(assert-equal value \"valuable-value\")
            </div>")
  (add-tal *test-generator* "test"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 xmlns:param=\"http://common-lisp.net/project/bese/tal/params\"
                 tal:in-package=\"talcl-test\">
               <tal:include tal:name=\"basic\" >
                 <param:value param:type=\"string\">valuable-value</param:value>
               </tal:include>
               <tal:include tal:name=\"basic\" >
                 <param:value tal:type=\"string\">valuable-value</param:value>
               </tal:include>
           </div>")
  (talcl:call-template-with-tal-environment *test-generator* "test" ()))

(defvar *test-answer*)
(adwtest test-include-param-body-as-string2 (compile-tests)
  (setf *test-count* 0
	*test-answer* "<div><span>valuable-value</span></div>")
  
  (add-tal *test-generator* "basic"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                 $(assert-true (stringp value))
                 $(assert-equal value *test-answer*)
                  $value
            </div>")
  (add-tal *test-generator* "test"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 xmlns:param=\"http://common-lisp.net/project/bese/tal/params\"
                 tal:in-package=\"talcl-test\">
               <tal:include tal:name=\"basic\" >
                 <param:value param:type=\"string\"><div><span>valuable-value</span></div></param:value>
               </tal:include>
           </div>")
  (talcl:call-template-with-tal-environment *test-generator* "test" ()))


(adwtest test-include-param-body-side-effects (compile-tests)
  (setf *test-count* 0)
  (add-tal *test-generator* "basic"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\">
                ${ (assert-equal *test-count* 1) }
                This is included content param:${value}
                <div class=\"content\" tal:content=\"value\" />
                <tal:tal tal:replace=\"value\" />
            </div>")
  (add-tal *test-generator* "test"
	   "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                 xmlns:param=\"http://common-lisp.net/project/bese/tal/params\"
                 tal:in-package=\"talcl-test\"
                 tal:let=\"pname2 'second pname3 'third\">
               <tal:include tal:name=\"basic\" >
                  <param:value>${(incf *test-count*)}</param:value>
               </tal:include>
           </div>")
  (talcl:call-template-with-tal-environment *test-generator* "test" ())
  (assert-equal *test-count* 1
		"We included 1 time so we should have incf'ed 3 times"))

(adwtest test-tal/lisp-escaping-body ()
  "Test tal expressions in body areas"
  (let* ((t1 "
          <div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               tal:let=\"x 1 y 'second z 'third\"
          >${ value }$$value$(princ-to-string value)$$(dont-eval)</div>")
	 (t2 "
          <div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               tal:let=\"x 1 y 'second z 'third\"
          >$value$$value${value}$$(dont-eval)</div>")
	 (fn1 (talcl::compile-tal-string t1))
	 (fn2 (talcl::compile-tal-string t2))
	 (out1 (talcl::buffer-xml-output ()
		 (talcl::%call-template-with-tal-environment fn1 (tal-env 'value 1))))
	 (out2 (talcl::buffer-xml-output ()
		 (talcl::%call-template-with-tal-environment fn2 (tal-env 'value 1)))))
    (tal-log.info "~%Out1:~s~%~%out2:~s" out1 out2 )
    (assert-equalp out1 out2)
    ))

(adwtest test-tal/lisp-escaping-attribs ()
  "Test tal expressions in attributes"
  (let* ((t1 "
          <div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               foo=\"${ value }\" bar=\"$$value\" bast=\"$(princ-to-string value)\"
               brocolli=\"$$(dont-eval)\" />")
	 (t2 "
          <div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"
               foo=\"$value\" bar=\"$$value\" bast=\"${value}\"
               brocolli=\"$$(dont-eval)\" />")
	 (fn1 (talcl::compile-tal-string t1))
	 (fn2 (talcl::compile-tal-string t2))
	 (out1 (talcl::buffer-xml-output ()
		 (talcl::%call-template-with-tal-environment fn1 (tal-env 'value 1))))
	 (out2 (talcl::buffer-xml-output ()
		 (talcl::%call-template-with-tal-environment fn2 (tal-env 'value 1)))))
    (tal-log.info "~%Out1:~s~%~%out2:~s" out1 out2 )
    (assert-equalp out1 out2)
    ))

(adwtest test-def (compile-tests)
  (let* ((it "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\"
                   tal:let=\"y 3 cnt 0\">
                 <tal:def tal:name=\"test-def\">
                    <span tal:let=\"x 3\">
                     Test is: ${ (assert-equal x y) }
                     ${ (incf cnt) }
                    </span>
                 </tal:def>
                 ${ (assert-true test-def) }
                 ${ test-def }
                 ${ test-def }
                 currently def is run once at definition and includable as a string thereafter
                 ${ (assert-eql cnt 1) }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-def-string (compile-tests)
  (let* ((it "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\"
                   tal:let=\"y 3\">
                 <tal:def tal:name=\"test-def\" tal:type=\"string\">
                    This is a string full of text, not some stupid xml yuck $y
                 </tal:def>
                 <tal:def tal:name=\"test-def2\" tal:type=\"string\">Yippie</tal:def>
                 ${ (assert-true (stringp test-def)) }
                 ${ (assert-true (stringp test-def2)) }
                 ${ (assert-equalp test-def2 \"Yippie\") }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))


(adwtest test-tal/print-env (smoke-tests)
  "Run print-env handler"  
  (tal-log.info "print-env output:~%~s"
		(talcl::buffer-xml-output ()
		  (talcl::%call-template-with-tal-environment
		   (talcl::compile-tal-string "
          <div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\"><tal:print-env />
${ (assert-true T) }
</div>")
		   (tal-env 'value 1)))))


(adwtest test-other-ns-passthrough (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\">
<foo:bar xmlns:foo=\"http://foo.bar/\"><foo:baz foo:data=\"jack\">hahaha</foo:baz></foo:bar>
</div>")
	 (fn (talcl::compile-tal-string it))
	 (out (buffer-xml-output ()
		(talcl::%call-template-with-tal-environment fn nil))))
    (assert-true
     (search "xmlns:foo=\"http://foo.bar/\"" out :test #'char=)
     out)
    (assert-true
     (search "foo:data=\"jack\"" out :test #'char=)
     out)
    (assert-true
     (search "hahaha</foo:baz>" out :test #'char=)
     out)))

(adwtest test-dom-insertion (dom)
  "Verify that when we insert nodes directly into the dom,
   that they appear and have the correct content and locations"
  (let* ((tmpl "<html
               class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"> first lb
<head><title>Title</title></head> second lb
<body><div id=\"header\" class=\"my-class\"><h1>Title</h1></div></body> third lb
</html>")
	 (_ (add-tal *test-generator* "basic-window" tmpl))
	 (doc (buildnode:with-xhtml-document
		(tal-template-content *test-generator* "basic-window" nil)))
	 (ds (document-to-string doc)))
    (declare (ignore _))
    (assert-equalp
     "HTML"
     (dom:tag-name (dom:first-child doc))
     doc
     ds)

    (assert-equalp
     "welcome frontPageMenu"
     (buildnode:get-attribute (dom:first-child doc) :class)
     doc
     ds)

    (assert-equalp
     "Title"
     (buildnode:text-of-dom-snippet
       (elt (dom:child-nodes (dom:first-child doc)) 1))
     doc
     ds)
    
    ))

(adwtest test-document-to-string (dom)
  ;;                xmlns=\"http://www.w3.org/1999/xhtml\"
  (add-tal *test-generator* "basic-window"
	   "<html
               class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"> first lb
<head><title>Page Title</title></head> second lb
<body><div id=\"header\" class=\"my-class\"><h1>Title</h1></div></body> <!-- third lb -->
</html>")
  (add-tal *test-generator* "basic-window-guts"
	   "<tal:tal
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"> first lb
<head><title>Page Title</title></head> second lb
<body><div id=\"header\" class=\"my-class\"><h1>Title</h1></div></body> <!-- third lb -->
</tal:tal>")
  (let* (;; TODO: Figure out how to get attributes to round trip in the same order
	 (match-in-order
	  "<body><div id=\"header\" class=\"my-class\"><h1>Title</h1></div></body> <!-- third lb -->")
	 (match "<body><div class=\"my-class\" id=\"header\"><h1>Title</h1></div></body> <!-- third lb -->")
	 
	 ;; inserting a template as a dom sub-tree
	 (doc (buildnode:with-xhtml-document
		(tal-template-content *test-generator* "basic-window" nil)))
	 (ds (talcl:document-to-string doc))

	 ;; inserting a template as a dom sub-tree
	 (doc3 (buildnode:with-xhtml-document		 
		 (tal-template-content *test-generator* "basic-window-guts" nil
				       (xhtml:html ()))))
	 (ds3 (talcl:document-to-string doc3))

	 ;; Inserting a template as a processing instruction
	 (doc2 (buildnode:with-xhtml-document
		(tal-processing-instruction *test-generator* "basic-window" nil)))
	 (ds2 (talcl:document-to-string doc2))
	 
	 ;; inserting a template as a dom sub-tree
	 (doc4 (buildnode:with-html-document
		 (tal-template-content *test-generator* "basic-window-guts" nil
				       (xhtml:html ()))))
	 (ds4 (talcl:document-to-string doc4))
	 
	 ;; inserting a template as a dom sub-tree
	 (doc5 (buildnode:with-html-document
		 (tal-template-content *test-generator* "basic-window" nil)))
	 (ds5 (talcl:document-to-string doc5)))
    
    (assert-true
     (search match ds :test #'string-equal)
     match
     doc
     ds
     :dom-insert)

    (assert-true
     (search match ds3 :test #'string-equal)
     match
     doc3
     ds3
     :dom-insert-guts)

    (assert-true
     (search match-in-order ds2 :test #'string-equal)
     match-in-order
     doc2
     ds2
     :processing-instruction)
    
    (assert-true
     (search match ds4 :test #'string-equal)
     match
     doc4
     ds4
     :dom-insert-guts-html)
    (assert-true
     (search match ds5 :test #'string-equal)
     match
     doc5
     ds5
     :dom-insert-html)))

