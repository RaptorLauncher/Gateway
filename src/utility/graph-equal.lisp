;;; TODO utilize
;;; pjb's graph-equal utility
;;; license: AGPLv3

#|

node-id

graph of nodes
nodes have an integer ID such as (= (node-id node1)  (node-id node2)) <=> (eql node1 node2)
edges are directed
edges are tagged
(edges-tagged graph node tag) -> list-of-edges



(graph-nodes graph) -> list-of-nodes

(edges-tagged graph node tag) -> list-of-edges

(node-id node) -> integer

(edge-tag edge) -> tag
(edge-from edge) -> node
(edge-to edge) -> node

|#




(defgeneric graph-nodes  (graph)          (:documentation "Returns a list of nodes."))
(defgeneric edges-tagged (graph node tag) (:documentation "Returns a list of edges."))
(defgeneric edge-tags    (graph node)     (:documentation "Returns a list of edge tags from the node."))
(defgeneric node-id      (node)           (:documentation "Returns the integer ID of the node."))
(defgeneric edge-tag     (edge)           (:documentation "Returns the tag of the edge."))
(defgeneric edge-from    (edge)           (:documentation "Returns the from node of the edge."))
(defgeneric edge-to      (edge)           (:documentation "Returns the to node of the edge."))
(defgeneric nodep        (node)           (:documentation "Returns true when node is a node."))

(defun make-set (elements)
  (let ((set (make-hash-table)))
    (dolist (element elements set)
      (setf (gethash element set) t))))

(defun choose (set)
  (maphash (lambda (element present)
             (declare (ignore present))
             (return-from choose element))
           set))

(defun extract (set element)
  (remhash element set))

(defun contains (set element)
  (gethash element set))



(defun node-equal (a b)
  (or (eql a b)
      (= (node-id a) (node-id b))))

(defun node-set-equal (a b)
  (let ((nodes-a (sort (copy-list a) (function <) :key (function node-id)))
        (nodes-b (sort (copy-list b) (function <) :key (function node-id))))
    (values (and (= (length nodes-a) (length nodes-b))
                 (every (function node-equal) nodes-a nodes-b))
            nodes-a
            nodes-b)))

(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a)))

(defun split-nodes (elements)
  (loop
    :for element :in elements
    :if (nodep element)
      :collect element :into nodes
    :else
      :collect element :into others
    :finally (return (values nodes others))))

(defun graph-equal (a b)
  (multiple-value-bind (node-set-equal nodes-a nodes-b) (node-set-equal (graph-nodes a) (graph-nodes b))
    (when node-set-equal
      (loop
        :while nodes-a
        :for node-a := (pop nodes-a)
        :for node-b := (pop nodes-b)
        :do (unless (node-equal node-a node-b)
              (return-from graph-equal nil))
            (let ((tags-a (edge-tags a node-a))
                  (tags-b (edge-tags b node-b)))
              (unless (set-equal tags-a tags-b)
                (return-from graph-equal nil))
              (loop
                :for tag-a :in tags-a
                :for edges-a := (edges-tagged a node-a tag-a)
                :for edges-b := (edges-tagged b node-b tag-b)
                :for tos-a := (mapcar (function edge-to) edges-a)
                :for tos-b := (mapcar (function edge-to) edges-b)
                :do (multiple-value-bind (to-nodes-a to-others-a) (split-nodes tos-a)
                      (multiple-value-bind (to-nodes-b to-others-b) (split-nodes tos-b)
                        (unless (set-equal to-others-a to-others-b)
                          (return-from graph-equal nil))
                        (unless (node-set-equal to-nodes-a to-nodes-b)
                          (return-from graph-equal nil))))))
        :finally (return-from graph-equal t)))))
