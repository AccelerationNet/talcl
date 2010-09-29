(in-package :talcl-test)
(cl-interpol:enable-interpol-syntax)

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
    (talcl:call-template-with-tal-environment *test-generator* "test" ())
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
                ${ (assert-true (typep value 'buffering-sink ))
                   (assert-true (%test-include-body-name
                                 *test-count*
                                 (talcl::buffer-xml-output ()
                                    (stop-buffering-and-flush value cxml::*sink*))))
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
                   tal:let=\"y 3\">
                 <tal:def tal:name=\"test-def\">
                    <span tal:let=\"x 3\">
                     Test is: ${ (assert-equal x y) }
                    </span>
                 </tal:def>
                 ${ (assert-true test-def) }
                 ${ test-def }
                 ${ test-def }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-def-string (compile-tests)
  (let* ((it "<div xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
                   tal:in-package=\"talcl-test\"
                   tal:let=\"y 3\">
                 <tal:def tal:name=\"test-def\" tal:type=\"string\">
                    This is a string full of text, not some stupid xml yuck
                 </tal:def>
                 <tal:def tal:name=\"test-def2\" tal:type=\"string\">Yippie</tal:def>
                 ${ (assert-true (stringp test-def)) }
                 ${ (assert-true (stringp test-def2)) }
                 ${ (assert-equalp test-def2 \"Yippie\") }
          </div>")
	 (fn (talcl::compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))