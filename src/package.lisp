(defpackage #:specl
  (:use #:cl)
  (:export ; src/globals
           #:*shared-contexts*
           ; src/util 
           #:alias #:dbind #:lazy-let
           ; src/env
           #:new-env #:with-env #:env? #:env+ #:inherit #:form->env #:forms->env
           ; src/context
           #:validate-context-syntax #:normalize-descs #:env->cl #:context
           ; src/shared-context
           #:validate-shared-context-syntax #:shared-context))
