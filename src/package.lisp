(defpackage #:specl.util
  (:use #:cl)
  (:export #:alias #:dbind #:lazy-let #:string-case #:first-by #:flatten #:is
           #:is-not))

(defpackage #:specl.tree
  (:use #:cl)
  (:export #:new-tree #:tree? #:value #:tree-children #:set-value #:add-child
           #:remove-children-if #:remove-children-unless #:add-child-unique
           #:map-tree #:map-with-accum #:tree->new-tree-syntax #:find-tree
           #:tree->list #:filter-tree))

(defpackage #:specl.globals
  (:use #:cl #:specl.tree)
  (:export #:*contexts* #:*shared-contexts* #:*behaviors*))

(defpackage #:specl.env
  (:use #:cl #:specl.util)
  (:export #:new-env #:with-env #:env? #:env+ #:inherit #:env->new-env-syntax
           ; Make sure we export the symbols captured by with-env
           #:env #:tags #:desc #:befores #:afters #:funcs #:macros #:lets
           #:expectation))

(defpackage #:specl.test
  (:use #:cl #:specl.util)
  (:export ))

(defpackage #:specl.syntax
  (:use #:cl #:specl.globals #:specl.util #:specl.env #:specl.tree)
  (:export #:validate-syntax #:normalize #:form->env-tree #:forms->env-tree
           #:env-tree->lambda-tree #:context #:shared-context #:behavior))

(defpackage #:specl.runner
  (:use #:cl #:specl.globals #:specl.util #:specl.tree)
  (:export #:run-tree #:run-all #:run-by-desc))

(defpackage #:specl
  (:use #:cl #:specl.globals #:specl.util #:specl.env #:specl.syntax
        #:specl.runner #:specl.tree)
  (:export #:context #:shared-context #:behavior #:is #:is-not #:run-all
           #:run-by-desc))
