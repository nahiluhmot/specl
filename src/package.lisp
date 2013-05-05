(defpackage #:specl
  (:use #:cl)
  (:export ; src/globals
           #:*shared-contexts*
           ; src/util 
           #:alias #:dbind #:lazy-let
           ; src/env
           #:new-env #:with-env #:env? #:env+ #:inherit
           ; src/context
           #:validate-context-syntax #:normalize-descs #:form->env #:forms->env
           #:env->cl #:context 
           #:validate-shared-context-syntax #:shared-context))
