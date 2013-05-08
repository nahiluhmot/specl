(defpackage #:specl-globals
  (:use #:cl)
  (:export #:*contexts* #:*shared-contexts* #:*behaviors* #:clear-globals!))

(defpackage #:specl-util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case))

(defpackage #:specl-env
  (:use #:cl #:specl-globals #:specl-util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit #:form->env
           #:forms->env))

(defpackage #:specl-syntax
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:validate-syntax #:normalize-descs #:context #:shared-context
           #:behavior))

(defpackage #:specl
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-syntax)
  (:export #:context #:shared-context #:behavior))
