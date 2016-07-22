
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)

;; quicklispのアップデート
(ql:update-client)

;; インストールしたライブラリのアップデート
(ql:update-all-dists)

;; ライブラリの検索
(ql:system-apropos "ライブラリの名前の一部")

;; ライブラリの読み込み（無ければインストールして読み込み）
(ql:quickload "ライブラリ名")
