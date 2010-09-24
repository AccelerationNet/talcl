(in-package :talcl-test)
(cl-interpol:enable-interpol-syntax)

(adwtest test-in-package (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
               ${ (assert-equal (find-package :talcl-test) *expression-package*) }
          </div>")
	 (fn (compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-let (compile-tests)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
                  <span tal:let=\"x 3\">
                   ${ (assert-equal x 3) }
                  </span>
          </div>")
	 (fn (compile-tal-string it)))
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
	 (fn (compile-tal-string it)))
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
	 (fn (compile-tal-string it)))
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
	 (fn (compile-tal-string it)))
    (talcl::%call-template-with-tal-environment fn ())))

(adwtest test-test-generator (compile-meta-tests compile-test)
  (let* ((it "<div class=\"welcome frontPageMenu\"
               xmlns:tal=\"http://common-lisp.net/project/bese/tal/core\"
               tal:in-package=\"talcl-test\">
               ${ (assert-true T \"This is just so we can be sure this ran\") }
          </div>")
	 (fn (compile-tal-string it)))
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
                <div class=\"content\" tal:content=\"value\" />
                <tal:tal tal:replace=\"value\" />
                ${ (assert-true (typep value 'function))
                   (assert-true (%test-include-body-name
                                 *test-count*
                                 (talcl::buffer-xml-output () (funcall value))))
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