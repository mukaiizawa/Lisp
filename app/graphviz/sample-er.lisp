
(load "er")

(defparameter tables
  (deftables
    (user_tbl
      "ユーザーマスタ"
      (user_id "ユーザーID" :primarykey? t :type "varchar2")
      (user_name "ユーザー名" :type "varchar2" :required? t))
    (group_tbl
      "部署テーブル"
      (group_id "部署ID" :primarykey? t  :type "varchar2")
      (group_name "部署名" :type "varchar2" :required? t))
    (user_group_tbl
      "ユーザー部署中間テーブル"
      (user_id "ユーザーID" :primarykey? t :foreignkey '(user_tbl user_id) :type "varchar2")
      (group_id "部署ID" :primarykey? t :foreignkey '(group_tbl group_id) :type "varchar2"))))

(dot (make-graph :nodes (tables->nodes tables)
                 :edges (tables->edges tables))
     :file "sample-er")

