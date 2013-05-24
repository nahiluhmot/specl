(in-package #:specl.spec)

(context "new-tree"
  (context "with no arguments"
    (subject (new-tree))

    (it "has a null value"
      (is (null (value subject))))
    (it "has no children"
      (is (null (tree-children subject)))))

  (context "when a value is supplied"
    (let test-value 15)
    (subject (new-tree :value test-value))

    (it "sets the value"
      (is (= test-value (value subject))))
    (it "has no children"
      (is (null (tree-children subject)))))

  (context "when children are supplied"
    (subject (new-tree :children children))

    (context "and they are all trees themselves"
      (let children (list (new-tree :value 'nonsense)
                          (new-tree :value 'test)))
      (it "sets the children"
        (is (equal children (tree-children subject)))))

    (context "but at least one of the children is not a tree"
      (let children (list 1 (new-tree :value 'uh-oh)))
      (it "raises an error"
        (is (handler-case (progn subject nil)
                          (error (e) t)))))))

(context "tree?"
  (context "when the argument is not a tree"
    (it "returns `nil`"
      (is-not (tree? nil))
      (is-not (tree? 'sym))
      (is-not (tree? '(1 2 3)))))
  (context "when the argument is a tree"
    (context "but at least one of its children is not a tree"
      (it "returns `nil`"
        (is-not (tree? '(tree nil (list 1))))))
    (context "and all of its children (if any) are trees"
      (it "returns `t`"
        (is (tree? (new-tree)))
        (is (tree? (new-tree :value 1)))
        (is (tree? (new-tree :children (list (new-tree)))))))))

(context "tree->new-tree-syntax"
  (subject (tree->new-tree-syntax form))

  (context "when the argument is not a tree"
    (let form 'not-a-tree)
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error subject)))

  (context "when the argument is a tree"
    (let tree (new-tree :value 1
               :children (list (new-tree :value 2)
                               (new-tree :value 3
                                         :children (list (new-tree :value 4)
                                                         (new-tree :value 5))))))
    (let form tree)

    (it "expands it"
      (is (equal tree (eval subject))))))

(context "value"
  (include-context "error-helpers")
  (context "when the argument is a tree"
    (let test-value 15)
    (subject (new-tree :value test-value))
    (it "returns the value"
      (is (eq test-value (value subject)))))
  (context "when the argument is not a tree"
    (subject 'not-a-tree)
    (it "raises an error"
      (raises-error (value subject)))))

(context "tree-children"
  (include-context "error-helpers")

  (context "when the argument is a tree"
    (let test-children (list (new-tree :value 1) (new-tree :value 2)))
    (subject (new-tree :children test-children))
    (it "returns its children"
      (is (equal test-children (tree-children subject)))))

  (context "when the argument is not a tree"
    (subject 'i-am-not-a-tree)
    (it "raises an error"
      (raises-error (tree-children subject)))))

(context "set-value"
  (include-context "error-helpers")
  (let test-value 'my-sweet-test-value)
  (subject (set-value test-value original-tree))

  (context "when the argument is a tree"
    (let children (list (new-tree :value 1)
                        (new-tree :value 2)))
    (let original-tree (new-tree :value 'old-value
                                 :children children))

    (it "returns a new tree with the specified value"
      (is (equal test-value (value subject))))
    (it "inherits the other tree's children"
      (is (equal (tree-children original-tree)
                 (tree-children subject)))))

  (context "when the argument is not a tree"
    (let original-tree 'not-a-tree)

    (it "raises an error"
      (raises-error subject))))

(context "add-child"
  (include-context "error-helpers")
  (subject (add-child child parent))

  (context "when both of the arguments are trees"
    (let parent (new-tree :value 1))
    (let child (new-tree :value 2))

    (it "adds the first tree to the second tree's children"
      (is (equal (value subject) (value parent)))
      (is (= 1 (length (tree-children subject))))
      (is (equal child (car (tree-children subject))))))

  (context "when the parent is not a tree"
    (let parent 'no-trees-here)
    (let child (new-tree))

    (it "raises an error"
      (raises-error subject)))

  (context "when the child is not a tree"
    (let parent (new-tree))
    (let child 'not-a-tree-brah)

    (it "raises an error"
      (raises-error subject))))

(context "map-tree"
  (include-context "error-helpers")
  (subject (map-tree #'1+ test-tree))

  (context "when the correct arguments are passed in"
    (let test-tree (new-tree :value 0
                             :children (list (new-tree :value 1)
                                             (new-tree :value 2))))
    (let expected-tree (new-tree :value 1
                                 :children (list (new-tree :value 2)
                                                 (new-tree :value 3))))

    (it "applys the function to each value in the tree"
      (is (equal subject expected-tree))))

  (context "when the second argument is not a tree"
    (let test-tree '(actually not a tree))
    (it "raises an error"
      (raises-error subject))))

(context "remove-children-if"
  (subject (remove-children-if #'pred tree))

  (defun pred (tree)
    (null (value tree)))

  (context "when the argument is not a tree"
    (let tree 'not-a-tree-bro)
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error subject)))

  (context "when the argument is a tree"
    (let tree (new-tree :value 1
                        :children (list (new-tree :value nil)
                                        (new-tree :value 2))))
    (let expected (new-tree :value 1
                            :children (list (new-tree :value 2))))
    (it "filters the children by the specified predicate"
      (is (equal subject expected)))))

(context "remove-children-unless"
  (subject (remove-children-unless #'pred tree))
  
  (defun pred (tree)
    (not (null (value tree))))

  (context "when the argument is not a tree"
    (let tree 'not-a-tree-bro)
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error subject)))

  (context "when the argument is a tree"
    (let tree (new-tree :value 1
                        :children (list (new-tree :value nil)
                                        (new-tree :value 2))))
    (let expected (new-tree :value 1
                            :children (list (new-tree :value 2))))
    (it "filters the children by the specified predicate"
      (is (equal subject expected)))))

(context "add-child-unique"
  (subject (add-child-unique child tree :test #'test))

  (let child (new-tree :value 1))
  (let existing-child (new-tree :value 1))
  (let tree (new-tree :value 2
                      :children (list existing-child
                                      (new-tree :value 3))))
  (defun test (a b)
    (equal a b))

  (context "when the child is not a tree"
    (let child 'not-a-tree)
    (include-context "error-helpers")

    (it "raises an error"
      (raises-error subject)))

  (context "when the tree is not a tree"
    (let tree 'not-a-tree)
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error subject)))

  (context "when both the child and tree are trees"
    (context "when none of the tree's children satisfy the predicate"
      (let existing-child (new-tree :value 4))
      (let expected (new-tree :value 2
                              :children (list (new-tree :value 1)
                                              (new-tree :value 4)
                                              (new-tree :value 3))))
      (it "just adds the new child"
        (is (equal subject expected))))

    (context "when at least one of the tree's children satisfies the predicate"
      (it "adds the new child and removes the children that satisfy the predicate"
        (is (equal subject tree))))))

(context "find-tree"
  (include-context "error-helpers")
  (subject (sort (find-tree #'oddp test-tree) #'<))

  (context "when the correct argumnets are passed in"
    (let test-tree
      (new-tree :value 1
                :children (list (new-tree :value 0
                                          :children (list (new-tree :value 3)
                                                          (new-tree :value 4)))
                                (new-tree :value 2))))
    (it "returns a list of every value that satisfies the predicate"
      (is (equal subject '(1 3)))))
  
  (context "when the second argument is not a tree"
    (let test-tree '(help me))
    (it "raises an error"
      (raises-error subject))))

(context "tree->list"
  (include-context "error-helpers")
  (subject (tree->list test-tree))

  (context "when the argument is a tree"
    (let test-tree
      (new-tree :value 1
                :children (list (new-tree :value 2
                                          :children (list (new-tree :value 3)
                                                          (new-tree :value 4)))
                                (new-tree :value 5
                                          :children (list (new-tree :value 6)
                                                          (new-tree :value 7))))))
    (it "returns a list of each value in the tree"
      (is (equal '(1 2 3 4 5 6 7) subject))))

  (context "when the argument is not a tree"
    (let test-tree '(tree -- just kidding not a tree))
    (it "raises an error"
      (raises-error subject))))

(context "filter-tree"
  (subject (filter-tree #'pred tree))

  (context "when the predicate returns `t` for the tree's value"
    (let tree (new-tree :value 1
                        :children (list (new-tree :value 2)
                                        (new-tree :value 4))))
    (defun pred (value)
      (oddp value))

    (it "returns the tree"
      (is (equal subject tree))))

  (context "when the predicate returns `nil` for the tree's value"
    (context "and all of its children get recursively filtered out"
      (let tree (new-tree :value 1
                          :children (list (new-tree :value 3)
                                          (new-tree :value 5
                                                    :children (list (new-tree :value 7))))))
      (defun pred (value)
        (evenp value))

      (it "returns `nil`"
        (is (null subject))))

    (context "and at least one of its children does not get filtered out"
      (let tree (new-tree :value 1
                          :children (list (new-tree :value '(2)
                                                    :children (list (new-tree :value 3)))
                                          (new-tree :value 4
                                                    :children (list (new-tree :value '(5))))
                                          (new-tree :value 6
                                                    :children (list (new-tree :value 7))))))
      (let expected (new-tree :value 1
                              :children (list (new-tree :value '(2)
                                                        :children (list (new-tree :value 3)))
                                              (new-tree :value 4
                                                        :children (list (new-tree :value '(5)))))))
      (defun pred (value)
        (listp value))

      (it "returns the original tree plus the children that did not get filtered"
        (is (equal expected subject))))))
