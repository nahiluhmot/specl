(in-package #:specl.tree)

(defun new-tree (&key value children)
  "Creates a new tree."
  (if (every #'tree? children)
    (list 'tree value children) 
    (error "specl.tree:new-tree expected :children to be trees, got ~A" children)))

(defun tree? (form)
  "Returns `t` if the given form is a tree, `nil` otherwise."
  (and (listp form)
       (= 3 (length form))
       (eq 'tree (car form))
       (listp (third form))
       (every #'tree? (third form))
       t))

(defun tree->new-tree-syntax (tree)
  "Given a tree, retruns the Common Lisp syntax to create a new identical tree."
  (if (tree? tree)
    `(new-tree :value ,(value tree)
               :children (list ,@(mapcar #'tree->new-tree-syntax
                                         (tree-children tree))))
    (error "specl.tree:tree->new-tree-syntax expected a tree, got: ~A" tree)))

(defun value (tree)
  "Given a tree, returns its value."
  (if (tree? tree)
    (cadr tree)
    (error "specl.tree:value expected a tree, got: ~A" tree)))

(defun tree-children (tree)
  "Given a tree, returns a list of its children."
  (if (tree? tree)
    (caddr tree)
    (error "specl.tree:tree-children expected a tree, got: ~A" tree)  ))

(defun set-value (value tree)
  "Given a tree and a value, returns a new tree that is identical to the
original tree with the exception that its value is the new value."
  (new-tree :value value :children (tree-children tree)))

(defun add-child (child-tree tree)
  "Given a two trees, adds the second tree to the first trees children."
  (if (every #'tree? (list tree child-tree))
    (new-tree :value (value tree) :children (cons child-tree (tree-children tree)))
    (error "specl.tree:add-child expected two trees, got: ~A and ~A" tree child-tree)))

(defun remove-children-if (pred tree)
  "Given a predicate and tree, will remove each child in the tree that does not
satisfy the predicate."
  (if (tree? tree)
    (new-tree :value (value tree)
              :children (remove-if pred (tree-children tree)))
    (error "specl.tree:filter-tree expected a tree, got: ~A" tree)))

(defun remove-children-unless (pred tree)
  "Given a predicate and tree, will remove each child in the tree that does
satisfy the predicate."
  (remove-children-if (lambda (child) (not (funcall pred child))) tree))

(defun add-child-unique (child tree &key (test #'eql))
  "Given a child, tree, and optional test function, will add the child to the
tree, ensuring its uniqueness by the test function."
  (add-child child
             (remove-children-if (lambda (other-child)
                                   (funcall test child other-child))
                                 tree)))

(defun map-tree (func tree)
  "Given a function with an arity of one and a tree, returns a new tree with the
function applied to each value in the tree."
  (if (tree? tree)
    (new-tree :value (funcall func (value tree))
              :children (mapcar (lambda (child) (map-tree func child))
                                (tree-children tree)))
    (error "specl.tree:map-tree expected a tree, got: ~A" tree)))

(defun map-with-accum (func tree &key initial-value)
  (multiple-value-bind (new-node next-value) (funcall func (value tree) initial-value)
    (new-tree :value new-node
              :children (mapcar (lambda (child)
                                  (map-with-accum func child :initial-value next-value))
                                (tree-children tree)))))

(defun find-tree (pred tree)
  "Given a predicate and a tree, returns a list of values in the tree that
satisfy the predicate."
  (if (tree? tree)
    (let ((found-children (reduce #'append
                                  (mapcar (lambda (child) (find-tree pred child))
                                          (tree-children tree)))))
      (if (funcall pred (value tree))
        (cons (value tree) found-children)
        found-children))
    (error "specl.tree:find-tree expected a tree, got: ~A" tree)))

(defun tree->list (tree)
  "Traverses the tree and produces a list."
  (if (tree? tree)
    (cons (value tree) (reduce #'append (mapcar #'tree->list (tree-children tree))))
    (error "specl.tree:tree->list expected a tree, got: ~A" tree)))

(defun filter-tree (pred tree)
  "Given a predicate and a tree, will filter the tree based upon the predicate.
If the predicate returns a non-nil value on the tree's value, the original tree
is returned. If the predicate returns a non-nil value on any of the tree's
children (or any of their children) a new tree with the children that satisfy
the predicate is returned. Otherwise, nil is returned."
  (if (tree? tree)
    (if (funcall pred (value tree))
      tree
      (let ((children (remove nil
                              (mapcar (lambda (child) (filter-tree pred child))
                                      (tree-children tree)))))
        (if (null children)
          nil
          (new-tree :value (value tree)
                    :children children))))
    (error "specl.tree:filter-tree expected a tree, got: ~A" tree)))
