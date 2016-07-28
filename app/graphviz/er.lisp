
(load "graphviz")

(set-attr! 'shape "plaintext" *global-node-conf*)
(set-attr! 'arrowhead "vee" *global-edge-conf*)

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
                                             ,@(rest col)))
                                        columns))))

(defmacro deftables (&rest tables)
  `(list
     ,@(mapcar (lambda (table) 
                 `(deftable ,(first table)
                    ,@(rest table)))
               tables)))

(defun tables->nodes (tables)
  (let (nodes)
    (dolist (table tables)
      (let* ((table-name (mkstr (table-name table)))
             (attr (list
                     (list 'label
                           (with-output-to-string (attr)
                             (format attr "<<TABLE BORDER='0' CELLBORDER='1' CELLSPACING='0'>")
                             (format attr "<TR><TD BGCOLOR='GRAY' WIDTH='200'>~A</TD></TR>" table-name)
                             (dolist (column (table-columns table))
                               (format attr "<TR><TD PORT='~A' ALIGN='LEFT'><FONT COLOR='~A'>~A</FONT></TD></TR>"
                                       (mkstr table-name "_" (column-phisical-name column))
                                       (if (column-primarykey? column) "RED" "BLACK")
                                       (column-phisical-name column)))
                             (format attr "</TABLE>>"))))))
        (push (make-node :value table-name :attr attr) nodes)))
    (nreverse nodes)))

(defun tables->edges (tables)
  (let (edges)
    (dolist (table tables)
      (dolist (column (table-columns table))
        (awhen (column-foreignkey column)
          (let* ((table-name-from (table-name table))
                 (port-name-from (mkstr table-name-from "_" (column-phisical-name column)))
                 (table-name-to (first (column-foreignkey column)))
                 (port-name-to (mkstr table-name-to "_" (second (column-foreignkey column)))))
            (push (make-edge :from (mkstr table-name-from ":" port-name-from)
                             :to (mkstr table-name-to ":" port-name-to))
                  edges)))))
    (nreverse edges)))

