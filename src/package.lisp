(defpackage #:specl-globals
  (:use #:cl)
  (:export #:*contexts* #:*shared-contexts* #:*behaviors* #:clear-globals!))

(defpackage #:specl-util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case #:is #:is-not))

(defpackage #:specl-env
  (:use #:cl #:specl-globals #:specl-util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit #:form->env
           #:forms->env))

(defpackage #:specl-syntax
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:validate-syntax #:normalize #:context #:shared-context #:behavior))

(defpackage #:specl
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-syntax)
  (:export #:context #:shared-context #:behavior #:is #:is-not))
