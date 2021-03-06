(require :stdlib *module-stdlib*)
(require :graph-utils *module-graph-utils*)
(provide :database-manager)

(defstruct schema
  (phisical-name "" :type string)
  (logical-name "" :type string)
  (tables nil :type list))

(defstruct table
  (phisical-name "" :type string)
  (logical-name "" :type string)
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

(defmacro defschema (schema-phisical-name schema-logical-name &rest tables)
  `(make-schema
     :phisical-name (mkstr ',schema-phisical-name)
     :logical-name (mkstr ',schema-logical-name)
     :tables (list ,@(mapcar (lambda (table) 
                               `(deftable ,(first table)
                                  ,@(rest table)))
                             tables))))

;; utility
;; get-table-name {{{

(defun get-table-name (schema table)
  (mkstr (schema-phisical-name schema) #\.  (table-phisical-name table)))

;; }}}
;; get-primarykeys {{{

(defun get-primarykeys (table)
  (mapcar #'column-phisical-name
          (remove-if (lambda (column)
                       (not (column-primarykey? column)))
                     (table-columns table))))

;; }}}
;; get-foreignkeys {{{

(defun get-foreignkeys (table)
  (filter (lambda (column)
            (awhen (column-foreignkey column)
              (list (column-phisical-name column) it)))
          (table-columns table)))

;; }}}

;; extension
;; schema->nodes {{{

(defun schema->nodes (schema &optional with-logical-table-name? with-logical-column-name?)
  (let (nodes)
    (dolist (table (schema-tables schema))
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
;; schema->edges {{{

(defun schema->edges (schema)
  (let (edges)
    (dolist (table (schema-tables schema))
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
;; schema->graph {{{

(defun schema->graph (schema)
  (make-graph :nodes (schema->nodes schema)
              :edges (schema->edges schema)))

;; }}}
;; schema->create-sql {{{

(defun schema->create-sql (schema)
  (with-output-to-string (out)
    (dolist (table (schema-tables schema))
      (format out "~%/* ~A */" (table-logical-name table))
      (format out "~%CREATE TABLE ~A (~%" (table-phisical-name table))
      (write-string
        (list->string
          (append
            (mapcar (lambda (column)
                      (mkstr (column-phisical-name column)
                             #\Space
                             (column-type column)
                             (mkstr-if (/= (column-length column) -1)
                               "(" (column-length column) ")")
                             (mkstr-if (not (empty? (column-default-value column)))
                               " DEFAULT " (column-default-value column))
                             (when (column-required? column)
                               " NOT NULL")))
                    (table-columns table))
            (aand (get-primarykeys table)
                  (list (mkstr "CONSTRAINT PK_" (table-phisical-name table) " PRIMARY KEY (" (list->string it ",") ")")))
            (mapcar (lambda (foreignkeys no)
                      (let* ((cols (mapcar #'first (rest foreignkeys)))
                             (refer-table (first foreignkeys))
                             (refer-cols (mapcar (compose #'second #'second) (rest foreignkeys)))
                             (fk (lambda (no cols refer-cols)
                                   (format nil "CONSTRAINT FK_~A~A FOREIGN KEY (~A) REFERENCES ~A(~A) ON DELETE CASCADE"
                                           (table-phisical-name table) no cols refer-table refer-cols))))
                        (if (< (length (remove-duplicates refer-cols :test #'string=)) (length refer-cols))
                          (list->string 
                            (mapcar (lambda (i)
                                      (funcall fk (mkstr no "_" i) (nth (1- i) cols) (first refer-cols)))
                                    (iota 1 (length refer-cols)))
                            (mkstr "," #\newline))
                          (funcall fk no (list->string cols #\,) (list->string refer-cols #\,)))))
                    (group-by (compose #'first #'second) (get-foreignkeys table))
                    (iota 1 (length (group-by (compose #'first #'second) (get-foreignkeys table))))))
          (mkstr #\, #\Newline))
        out)
      (format out ");~%"))))

;; }}}
;; schema->create-sql! {{{

(defun schema->create-sql! (schemas)
  (list->string
    (flatten
      (mapcar (lambda (schema)
                (mapcar (lambda (table)
                          (format nil "DROP TABLE ~A CASCADE CONSTRAINTS PURGE;~%~A"
                                  (get-table-name schema table)
                                  (schema->create-sql schema)))
                        (schema-tables schema)))
              (mklist schemas)))))

;; }}}
;; schemas->doc {{{

(defun schemas->doc (schemas &key (with-title t) (with-header t) (segment #\Tab))
  (dolist (schema (mklist schemas))
    (let ((schema-name (schema-phisical-name schema))
          (tables (schema-tables schema)))
      (mkdir schema-name)
      (dolist (table tables)
        (with-open-file (out (mkstr schema-name "/" (table-phisical-name table))
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (when with-title
            (princln (list->string
                       (list
                         (table-phisical-name table)
                         (table-logical-name table))
                       segment)
                     out))
          (when with-header
            (princln (list->string
                       '(phisical-name
                          logical-name
                          data-type
                          length
                          primarykey?
                          required?
                          foreignkey
                          default-value
                          remarks)
                       segment)
                     out))
          (dolist (column (table-columns table))
            (princln (list->string
                       (list
                         (column-phisical-name column)
                         (column-logical-name column)
                         (column-type column)
                         (column-length column)
                         (column-primarykey? column)
                         (or (column-primarykey? column)
                             (column-required? column))
                         (mkstr-aif (column-foreignkey column)
                           (first it) "." (second it))
                         (column-default-value column)
                         (column-remarks column))
                       segment)
                     out)))))))

;; }}}
