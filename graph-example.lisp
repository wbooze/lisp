(defstruct node (name "") (children nil))

(defvar g1 (let* ((2a (make-node :name "2A"))
                  (2b (make-node :name "2B"))
                  (2c (make-node :name "2C"))
                  (1a (make-node :name "1A" :children (list 2a 2b)))
                  (1b (make-node :name "1B" :children (list 2b 2c)))) 
             (make-node :name "0" :children (list 1a 1b))))

(defun test-graph (root-node &rest keys) 
  (apply #'clim:format-graph-from-root root-node 
         #'(lambda (node s)
             (write-string (node-name node) s)) 
         #'node-children keys))

;;(test-graph g1 *standard-output*)
;;(test-graph g1 *standard-output* :orientation :vertical)

(test-graph g1 :stream *standard-output*)
(test-graph g1 :stream *standard-output* :orientation :vertical)

