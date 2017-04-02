#-(and) "

P62B (*) Collect the nodes at a given level in a list

    A node of a binary tree is at level N if the path from the root to
    the node has length N-1. The root node is at level 1. Write a
    predicate atlevel/3 to collect all nodes at a given level in a
    list.
   
    % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
   
    Using atlevel/3 it is easy to construct a predicate levelorder/2
    which creates the level-order sequence of the nodes. However,
    there are more efficient ways to do that.
"

;; Simple recursive solution:

;; Notice the above definition use a 1-based index for the level.
;; As always, this is a bad choice, hence the use of (= 1 level) instead of (zerop level).
;; http://www.cs.utexas.edu/users/EWD/ewd08xx/EWD831.PDF


(defun collect-nodes-at-level (tree level)
  (cond
    ((binary-tree-empty-p tree) '())
    ((= 1 level)             (list tree))
    (t  (append (collect-nodes-at-level (binary-tree-left  tree) (1- level))
                (collect-nodes-at-level (binary-tree-right tree) (1- level))))))

;; Note: nconc could be used instead of append, since all the lists
;;       returned by collect-nodes-at-level are newly allocated lists.


;;;; THE END ;;;;
