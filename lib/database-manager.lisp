
(load "stdlib" :if-does-not-exist nil)

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
          (prune (lambda (column)
                   (not (column-primarykey? column)))
                 (table-columns table))))

;; }}}
;; tables->create-sql {{{

(defun tables->create-sql (tables)
  (with-output-to-string (out)
    (dolist (table (mklist tables))
      (format out "~%DROP TABLE ~A CASCADE CONSTRAINTS;~%" (get-table-name table))
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
      (format out "CONSTRAINT PK_~A (~{~A~^,~})~%"
              (table-phisical-name table)
              (get-primarykeys table))
      (format out ");~%"))))

;; }}}

