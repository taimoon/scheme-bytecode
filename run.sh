diff <(scheme --script test-lisp.scm interpreter-base.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter-base.scm) expected-meta.txt

diff <(scheme --script test-lisp.scm interpreter.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter.scm) expected-meta.txt

diff <(scheme --script test-lisp.scm interpreter-nameless.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter-nameless.scm) expected-meta.txt

diff <(scheme --script test-lisp.scm interpreter-meaning.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter-meaning.scm) expected-meta.txt

diff <(scheme --script test-lisp.scm interpreter-register.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter-register.scm) expected-meta.txt

diff <(scheme --script test-lisp.scm interpreter-bytecode.scm) expected.txt
diff <(scheme --script test-meta-lisp.scm interpreter-bytecode.scm) expected-meta.txt
