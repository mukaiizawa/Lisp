
(load "graphviz")

(set-attr 'shape "record" *global-node-conf*)


; (dot (make-graph :nodes (append (make-records '(("q_and_a" "q_and_a_no" "title")) '((color "grey")))
;                                 (make-nodes '("a" "b" "c")))))

(defstruct table name columns)

(defstruct column
  (phisical-name "" :type string)
  (logical-name "" :type string)
  (primarykey? nil :type boolean)
  (type "" :type string)
  (length 0 :type number)
  (required? nil :type boolean)
  (foreignkey nil :type list))

(defmacro deftable (table-name &rest columns)
  `(make-table :name (mkstr ',table-name)
               :columns (list ,@(mapcar (lambda (col)
                                          `(make-column
                                             :phisical-name ,(mkstr (first col))
                                             ;; exclude phisical-name
                                             ,@(flatten (mapcar (lambda (col-exclude-phisical-name)
                                                                  (case (first col-exclude-phisical-name)
                                                                    (:length (list (first col-exclude-phisical-name) (parse-int (mkstr (second col-exclude-phisical-name)))))
                                                                    (:primarykey? col-exclude-phisical-name)
                                                                    (:required? col-exclude-phisical-name)
                                                                    (:foreignkey col-exclude-phisical-name)
                                                                    (t (list (first col-exclude-phisical-name) (mkstr (second col-exclude-phisical-name))))))
                                                                (group (rest col) 2)))))
                                        columns))))

