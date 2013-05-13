(defpackage #:specl-util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case #:flatten #:is #:is-not))

(defpackage #:specl-tree
  (:use #:cl)
  (:export #:new-tree #:tree? #:value #:tree-children #:set-value #:add-child
           #:map-tree #:find-tree #:tree->list))

(defpackage #:specl-globals
  (:use #:cl #:specl-tree)
  (:export #:*contexts* #:*shared-contexts* #:*behaviors* #:clear-globals!))

(defpackage #:specl-env
  (:use #:cl #:specl-globals #:specl-util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit
           ; Make sure we export the symbols captured by with-env
           #:desc #:befores #:afters #:funcs #:macros #:lets #:expectation
           #:children))

(defpackage #:specl-syntax
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-tree)
  (:export #:validate-syntax #:normalize #:form->env #:forms->env #:env->lambdas
           #:context #:shared-context #:behavior))

(defpackage #:specl-runner
  (:use #:cl #:specl-globals #:specl-util #:specl-tree)
  (:export #:run-tree #:run-all #:run-by-desc))

(defpackage #:specl
  (:use #:cl #:specl-globals #:specl-util #:specl-env #:specl-syntax
        #:specl-runner #:specl-tree)
  (:export #:clear-globals! #:context #:shared-context #:behavior #:is #:is-not
           #:run-all #:run-by-desc))
