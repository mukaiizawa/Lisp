
(load "graphviz")

(set-attr! 'shape "record" *global-node-conf*)
(set-attr! 'style "vee" *global-edge-conf*)

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
  (make-records (mapcar (lambda (table)
                          (cons (table-name table)
                                (mapcar (lambda (column)
                                          (column-phisical-name column))
                                        (table-columns table))))
                        tables)))

(defun tables->edges (tables)
  (let (acc)
    (dolist (table tables)
      (dolist (column (table-columns table))
        (awhen (column-foreignkey column)
          (push (list (table-name table) (first it)) acc))))
    (make-edges acc)))

