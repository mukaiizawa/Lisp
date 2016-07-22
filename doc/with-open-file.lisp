(load "../lib/stdlib")

;; output
(with-open-file (out "file-name"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist nil)
  (print "hello world" out))

;; input
(with-open-file (in "file-name"
                    :direction :input
                    :if-does-not-exist :error)
  (let ((line nil))
    (while (setq line (read-line in nil))
           (echo line))))

;; keyword parameter
;; :direction {{{

:input
;; 入力

:output
;; 出力

:io
;; 入出力

; }}}
;; :element-type {{{

;; }}}
;; :if-exists {{{

:error
;; file-errorが返る.

:new-version
;; デフォルト.より大きなバージョン番号が振られたファイルが作成される.

:rename
;; 既存のファイルがリネームされて,指定したファイル名のファイルが新規作成される.

:rename-and-delete
;; 既存のファイルはリネームされてdeleteされるがexpungeされない???そして指定したファイル名のファイルが新規作成される.

:overwrite
;; 既存のファイルに上書きする.open直後,ファイルポインタはファイルの先頭にある.

:append
;; 既存のファイルに追加書き込みする.open直後,ファイルポインタはファイルの末尾にある.

:supersede
;; 既存の新ファイルで置き換える.処理系は可能ならばストリームが閉じられるまで,既存ファイルを新ファイルで置き換えない.

nil
;; ファイルもストリームも作成しない.nilが返る.

;; }}}
;; :if-does-not-exist {{{

:error
;; file-errorが返る.

:create
;; 空ファイルが作成され,ファイルが既存であった場合のように処理が続行される.ただしif-existsで指定したdirectionは実行されない.

nil
;; ファイルもストリームも作成されない.nilが返る.

;; }}}



;; @stdlib.lisp

;; each-line
(with-open-file (in "with-open-file.lisp" :direction :input)
  (each-line (echo line)))

;; wirte-to
(write-to "hello.txt" "path/hoge.txt" :ff :windows)

;; read-from
(format t "~{~A~%~}" (read-from "with-open-file.lisp"))
;; <=>
(let ((vector (read-from "with-open-file.lisp" :vector t)))
  (dotimes (i (length vector))
    (format t "~A~%" (svref vector i))))
;; => print this file.



