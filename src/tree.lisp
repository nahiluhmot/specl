(in-package #:specl-tree)

(defun new-tree (&key value children)
  "Creates a new tree."
  (if (every #'tree? children)
    (list 'tree value children) 
    (error "specl-tree:new-tree expected :children to be trees, got ~A" children)))

(defun tree? (form)
  "Returns `t` if the given form is a tree, `nil` otherwise."
  (and (listp form)
       (= 3 (length form))
       (eq 'tree (car form))
       (listp (third form))
       (every #'tree? (third form))
       t))

(defun value (tree)
  "Given a tree, returns its value."
  (if (tree? tree)
    (cadr tree)
    (error "specl-tree:value expected a tree, got: ~A" tree)))

(defun tree-children (tree)
  "Given a tree, returns a list of its children."
  (if (tree? tree)
    (caddr tree)
    (error "specl-tree:tree-children expected a tree, got: ~A" tree)  ))

(defun set-value (value tree)
  "Given a tree and a value, returns a new tree that is identical to the
original tree with the exception that its value is the new value."
  (new-tree :value value :children (tree-children tree)))

(defun add-child (child-tree tree)
  "Given a two trees, adds the second tree to the first trees children."
  (if (every #'tree? (list tree child-tree))
    (new-tree :value (value tree) :children (cons child-tree (tree-children tree)))
    (error "specl-tree:add-child expected two trees, got: ~A and ~A" tree child-tree)))

(defun map-tree (func tree)
  "Given a function with an arity of one and a tree, returns a new tree with the
function applied to each value in the tree."
  (if (tree? tree)
    (new-tree :value (funcall func (value tree))
              :children (mapcar (lambda (child) (map-tree func child))
                                (tree-children tree)))
    (error "specl-tree:map-tree expected a tree, got: ~A" tree)))

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
    (error "specl-tree:find-tree expected a tree, got: ~A" tree)))

(defun tree->list (tree)
  "Traverses the tree and produces a list."
  (if (tree? tree)
    (cons (value tree) (reduce #'append (mapcar #'tree->list (tree-children tree))))
    (error "specl-tree:tree->list expected a tree, got: ~A" tree)))
