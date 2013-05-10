(defpackage #:specl-globals
  (:use #:cl)
  (:export #:*contexts* #:*shared-contexts* #:*behaviors* #:*passes*
           #:*failures* #:clear-globals!))

(defpackage #:specl-util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case #:is #:is-not))

(defpackage #:specl-env
  (:use #:cl #:specl-globals #:specl-util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit
           ; Make sure we export the symbols captured by with-env
           #:desc #:befores #:afters #:funcs #:macros #:lets #:expectations
           #:children))

(defpackage #:specl-syntax
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:validate-syntax #:normalize #:form->env #:forms->env #:context
           #:shared-context #:behavior))

(defpackage #:specl-runner
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:expand-it #:expand-env #:run!))

(defpackage #:specl
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-syntax
        #:specl-runner)
  (:export #:clear-globals! #:context #:shared-context #:behavior #:is #:is-not
           #:run!))
