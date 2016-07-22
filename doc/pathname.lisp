(load "../lib/stdlib")

;; pathname からファイル名を取得
(file-namestring #P"/tmp/hoge.tgz")
;; => "hoge.tgz"

;; pathname から拡張子を取得
(pathname-type #P"/tmp/hoge.tgz")
;; => "tgz"

;; pathname からファイル名(拡張子無し)を取得
(pathname-name #P"/tmp/hoge.tgz")
;; => "hoge"

;; pathname からディレクトリを取得
(pathname-directory #P"/tmp/hoge.tgz")
;; => (:ABSOLUTE "tmp")

(pathname-directory #P"tmp/hoge.tgz")
;; => (:RELATIVE "tmp")

;; pathname からドライブレターを取得
(pathname-device #P"D:/tmp/hoge.tgz")
;; => "D"
