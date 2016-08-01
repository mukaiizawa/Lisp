
(load "../../lib/database-manager")
(load "graphviz")

(set-attr! 'shape "plaintext" *global-node-conf*)

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

