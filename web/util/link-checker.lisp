
(load "../../lib/stdlib")

;; html形式の文字列からハイパーリンク部をすべて抽出してリストにして返す
;; args:"<html> ... <a href="../asdf/h.htm"> ... </html>"
(defun get-hyper-links (str)
  (labels ((fn (str acc)
               (let ((buf "")
                     (pre #\Space)
                     (in-atag? nil)
                     (in-href? nil)
                     (in-link? nil))
                 (dostring (c str)
                   (if (char/= c #\Space ) (setq buf (mkstr buf c)))
                   (cond ((and in-link?
                               (char= c #\"))
                          (push (mkstr (subseq buf 0 (1- (length buf)))) acc)    ; exclude last #\"
                          (setq buf ""
                                in-atag? nil
                                in-link? nil
                                in-href? nil))
                         ((and in-href?
                               (char= c #\"))
                          (setq buf "")
                          (setq in-link? t))
                         ((and in-atag?
                               (string-equal buf "href"))
                          (setq in-href? t))
                         ((and (char= pre #\<)
                               (char-equal c #\a))
                          (setq buf "")
                          (setq in-atag? t)))
                   (setq pre c))
                 (nreverse acc))))
    (fn str nil)))
; #p(get-hyper-links " <HEAD> <a href=\"Content-Type\" content=\"text/html charset=Shift_JIS\"> <a http-equiv=\"Content-Language\" content=\"ja\"> <a name=\"Generator\"  content=\"MSHTML 5.00.2314.1000\"> <a href\"Author\" content=\"2007.8.9 A.Sasaki\"> </HEAD>")

;; リンク元のパスとリンク先の相対パスをもらってリンク先の絶対パスを返す
;; return -> absolute-link-to
(defun relative->absolute (absolute-link-from relative-link-to)
    (let* ((absolute-link-from (butlast (string->list #\/ absolute-link-from)))
           (relative-link-to (string->list #\/ relative-link-to))
           (upper-depth (count-if (lambda (x)
                                    (string= x ".."))
                                  relative-link-to)))
      (list->string (append (if (> upper-depth 0)
                              (reverse (nthcdr upper-depth (reverse absolute-link-from)))
                              absolute-link-from)
                            (remove-if (lambda (dir)
                                         (or (string= dir ".")
                                             (string= dir "..")))
                                       relative-link-to))
                    :char #\/)))
;; test {{{
; #p(relative->absolute "D:/hogehoge/fuga/piyo/menu/menu2/main.html" "../../data/d1/d2/d3.html")
;; => "D:/hogehoge/fuga/piyo/data/d1/d2/d3.html"
; #p(relative->absolute "D:/hogehoge/fuga/piyo/menu/menu2/main.html" "../data/d1/d2/d3.html")
;; => "D:/hogehoge/fuga/piyo/menu/data/d1/d2/d3.html"
; #p(relative->absolute "D:/hogehoge/fuga/piyo/1/2/main.html" "../a.html")
;; => "D:/hogehoge/fuga/piyo/1/a.html"
;;
;; }}}

;; 内部リンクのうち無効なものを出力する
(defexe link-checker (--help)
  (encoding :enc :cp932 :ff :windows)
  (let ((valid-path-list nil)
        (link-list nil)
        (usage (usage :title "link-checker"
                      :desc  "Get hyper links that invalid path.")))
    (if --help (funcall usage))
    (mapfile (lambda (pathname)
               (push (mkstr pathname) valid-path-list))
             :recursive t)
    (mapfile (lambda (pathname)
               (mapcar (lambda (hyper-link)
                         (if (not (search "http" hyper-link))    ; exclude absolute link like a 'http://...'
                           (push (list (mkstr pathname) hyper-link (relative->absolute (mkstr pathname) hyper-link)) link-list)))
                       (get-hyper-links  (list->string (read-from pathname)))))
             :extension '(html htm)
             :recursive t)
    (echo "path, origin-hyper-link, invalid-hyper-link(converted relative)")
    (mapcar (lambda (x)
              (echo (first x) ", " (second x) ", " (third x)))
            (delete-if (lambda (link)
                         (member (third link) valid-path-list :test #'string-equal))
                       link-list)))
  (echo "finish"))
