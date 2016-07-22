
(load "../../lib/stdlib")
(load "../../lib/test-utils")
(load "../../etc/regex")


;; あるディレクトリ以下のtxtファイルから
;; あるキーワード[word1, word2 ... wordn]を探し出し、ノードを作成する
;; (defparameter *node-edges* 
;; '((file1 word1)
;;   (file2 word1)
;;   (file2 word2)
;;   (file2 word3)
;;   ...
;;   (filen word1))

(defparameter pat-num (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *node-edges* nil)
(defparameter *pat* nil)
(setq *pat* (list #\上 #\位 #\規 #\程 #\類))


(defun tr (str)
  (let (buf)
    (with-input-from-string (in str)
      (awhile (read-char in nil nil)
        (push (case it
                (#\　 #\Space)
                (#\＿ #\_)
                (#\ー #\-)
                (#\０ #\0)
                (#\１ #\1)
                (#\２ #\2)
                (#\３ #\3)
                (#\４ #\4)
                (#\５ #\5)
                (#\６ #\6)
                (#\７ #\7)
                (#\８ #\8)
                (#\９ #\9)
                (t it))
              buf)))
    (trimstr #\Space (coerce (nreverse buf) 'string))))

(defexe get-nodes ()
  (mapfile (lambda (pathname)
             (dolist (line (read-from pathname))
               ; (awhen (match? pat line)
               ; (push (list file-name (subseq line (first it) (second it)))
               ;       *node-edges*)))))
               ; (when (search "上位規定" (tr line))
               (when (search "上位規程類" line)
                 (push (list (pathname-name pathname) line) *node-edges*))))
           :extension 'txt
           :recursive t)
  (print *node-edges*))

