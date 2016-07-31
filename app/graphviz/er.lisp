
(load "graphviz")

(set-attr! 'shape "plaintext" *global-node-conf*)

(defstruct table
  (phisical-name "" :type string)
  (logical-name "" :type string)
  (schema "" :type string)
  (columns nil :type list))

(defstruct column
  (phisical-name "" :type string)
  (logical-name "" :type string)
  (primarykey? nil :type boolean)
  (foreignkey nil :type list)
  (required? nil :type boolean)
  (default-value "" :type string)
  (type "" :type string)
  (length 0 :type number)
  (remarks "" :type string))

(defmacro deftable (table-phisical-name table-logical-name &rest columns)
  `(make-table :phisical-name (mkstr ',table-phisical-name)
               :logical-name (mkstr ',table-logical-name)
               :columns (list ,@(mapcar (lambda (col)
                                          `(make-column
                                             :phisical-name ,(mkstr (first col))
                                             :logical-name ,(mkstr (second col))
                                             ,@(nthcdr 2 col)))
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
      (let* ((table-phisical-name (mkstr (table-phisical-name table)))
             (attr (list 'label
                         (with-output-to-string (out)
                           (format out "<<TABLE BORDER='0' CELLBORDER='1' CELLSPACING='0'>")
                           (format out "<TR><TD BGCOLOR='GRAY' WIDTH='200'>~A</TD></TR>" table-phisical-name)
                           (dolist (column (table-columns table))
                             (format out "<TR><TD PORT='~A' BGCOLOR='~A' ALIGN='LEFT'><FONT COLOR='~A'>~A</FONT></TD></TR>"
                                     (mkstr table-phisical-name "_" (column-phisical-name column))
                                     (if (column-primarykey? column)
                                       "#E0FFFF"
                                       "#FFFFFF")
                                     (if (or (column-primarykey? column)
                                             (column-required? column))
                                       "RED" "BLACK")
                                     (column-phisical-name column)))
                           (format out "</TABLE>>")))))
        (push (list table-phisical-name attr) nodes)))
    (nreverse nodes)))

(defun tables->edges (tables)
  (let (edges)
    (dolist (table tables)
      (dolist (column (table-columns table))
        (awhen (column-foreignkey column)
          (let* ((table-name-from (table-phisical-name table))
                 (port-name-from (mkstr table-name-from "_" (column-phisical-name column)))
                 (table-name-to (first (column-foreignkey column)))
                 (port-name-to (mkstr table-name-to "_" (second (column-foreignkey column)))))
            (push (list (mkstr table-name-from ":" port-name-from)
                        (mkstr table-name-to ":" port-name-to))
                  edges)))))
    (nreverse edges)))

