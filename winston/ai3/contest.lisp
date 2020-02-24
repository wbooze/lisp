;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

Trees are encoded as nested lists:  Leaf nodes are represented by
numbers that indicate their static value.  Nonleaf nodes are
represented by lists of their child nodes.

Consider this tree, for example:


              *               <-- Nonleaf nodes

       *             *        <-- Nonleaf nodes

    *     *       *     *     <-- Nonleaf nodes

   2 2   0 4     6 8   4 6    <-- Leaf nodes's static values

Representing this same tree as a nested list produces the
following:

(((2 2) (0 4)) ((6 8) (4 6)))

|#

;;;; DEFINE VARIOUS GAME TREES

(defvar three-ply '(((2 2) (0 4)) ((6 8) (4 6))))

(defvar reverse-three-ply '(((6 4) (8 6)) ((4 0) (2 2))))


(defvar four-ply '((((6 2) (3 3))
		    ((3 9) (4 7)))
		   (((2 2) (0 4))
		    ((6 8) (4 6)))))

(defvar reverse-four-ply '((((6 4) (8 6))
			    ((4 0) (2 2)))
			   (((7 4) (9 3))
			    ((3 3) (2 6)))))