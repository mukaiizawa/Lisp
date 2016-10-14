
(require :stdlib *module-stdlib*)
(require :graph-utils *module-graph-utils*)
(provide :database-manager)

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
  (length -1 :type number)
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

;; utility
;; get-table-name {{{

(defun get-table-name (table)
  (with-output-to-string (out)
    (unless (empty? (table-schema table))
      (princ (mkstr (table-schema table) ".") out))
    (princ (table-phisical-name table) out)))

;; }}}
;; get-primarykeys {{{

(defun get-primarykeys (table)
  (mapcar #'column-phisical-name
          (remove-if (lambda (column)
                       (not (column-primarykey? column)))
                     (table-columns table))))

;; }}}

;; extension
;; tables->nodes {{{

(defun tables->nodes (tables &optional with-logical-table-name? with-logical-column-name?)
  (let (nodes)
    (dolist (table tables)
      (let* ((table-phisical-name (mkstr (table-phisical-name table)))
             (attr-shape (list 'shape "plaintext"))
             (attr-label (list 'label
                               (with-output-to-string (out)
                                 (format out
                                         "<<TABLE BORDER='0' CELLBORDER='1' CELLSPACING='0'>
                                         <TR>
                                         <TD BGCOLOR='GRAY' WIDTH='200'>~A~A</TD>
                                         </TR>"
                                         table-phisical-name
                                         (mkstr-if with-logical-table-name?
                                           "(" (table-logical-name table) ")"))
                                         (dolist (column (table-columns table))
                                           (format out "<TR><TD PORT='~A' BGCOLOR='~A' ALIGN='LEFT'><FONT COLOR='~A'>~A~A</FONT></TD></TR>"
                                                   (mkstr table-phisical-name "_" (column-phisical-name column))
                                                   (if (column-primarykey? column)
                                                     "#E0FFFF"
                                                     "#FFFFFF")
                                                   (if (or (column-primarykey? column)
                                                           (column-required? column))
                                                     "RED"
                                                     "BLACK")
                                                   (column-phisical-name column)
                                                   (mkstr-if with-logical-column-name?
                                                     "(" (column-logical-name column) ")")))
                                         (format out "</TABLE>>")))))
             (push (list table-phisical-name (list attr-shape attr-label)) nodes)))
      (nreverse nodes)))

;; }}}
;; tables->edges {{{

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

;; }}}
;; tables->graph {{{

(defun tables->graph (tables)
  (make-graph :nodes (tables->nodes tables)
              :edges (tables->edges tables)))

;; }}}
;; tables->create-sql {{{

(defun tables->create-sql (tables)
  (with-output-to-string (out)
    (dolist (table (mklist tables))
      (format out "CREATE TABLE ~A (~%" (get-table-name table))
      (dolist (column (table-columns table))
        (write-string
          (mkstr (column-phisical-name column) " " (column-type column)
                 (when (/= (column-length column) -1)
                   (mkstr "(" (column-length column) ")"))
                 (unless (empty? (column-default-value column))
                   (mkstr " DEFAULT " (column-default-value column)))
                 (when (column-required? column)
                   " NOT NULL")
                 #\, #\Newline)
          out))
      (format out "CONSTRAINT PK_~A PRIMARY KEY (~{~A~^,~})~%"
              (table-phisical-name table)
              (get-primarykeys table))
      (format out ");~%"))))

;; }}}
;; tables->create-sql! {{{

(defun tables->create-sql! (tables)
  (list->string
    (mapcar (lambda (table)
              (format nil "DROP TABLE ~A CASCADE CONSTRAINTS PURGE;~%~A"
                      (get-table-name table)
                      (tables->create-sql table)))
            (mklist tables))))

;; }}}

