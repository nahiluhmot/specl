(defpackage #:specl-globals
  (:use #:cl)
  (:export #:*contexts* #:*shared-contexts*))

(defpackage #:specl-util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case))

(defpackage #:specl-env
  (:use #:cl #:specl-globals #:specl-util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit #:form->env
           #:forms->env))

(defpackage #:specl-context
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:validate-context-syntax #:normalize-descs #:context))

(defpackage #:specl-shared-context
  (:use #:cl #:specl-globals #:specl-util #:specl-env)
  (:export #:validate-shared-context-syntax #:shared-context))

(defpackage #:specl
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-context
        #:specl-shared-context)
  (:export #:context #:shared-context))
