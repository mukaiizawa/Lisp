
(load "er")

(defparameter tables
  (deftables
    (user
      (user_id :logical-name "ユーザーID" :primarykey? t :type "varchar2" :required? t)
      (user_name :logical-name "ユーザー名" :type "varchar2"))
    (group
      (group_id :logical-name "部署ID" :primarykey? t :foreignkey '(user user_id) :type "varchar2" :required? t)
      (group_name :logical-name "部署名" :type "varchar2"))))

(dot (make-graph :nodes (tables->nodes tables)
                 :edges (tables->edges tables))
     :file "sample-er")
#o'finish
