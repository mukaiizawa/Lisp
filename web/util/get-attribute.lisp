
(load "../../lib/stdlib")

(defmacro initialize ()
  `(progn (setq buf nil
                find-tag? nil
                find-attri-name? nil
                find-attri-value? nil)))

;; html形式の文字列から任意のタグのある属性をリストにして返す
(defun get-attribute (tag attri str)
  (labels ((fn (acc)
               (let ((buf "")
                     (find-tag? nil)
                     (find-attri-name? nil)
                     (find-attri-value? nil))
                 (dostring (c str)
                   (when (and (char/= c #\<)
                              (char/= c #\>)
                              (char/= c #\&)
                              (or find-attri-value?
                                  (and (char/= c #\Tab)
                                       (char/= c #\Space)
                                       (char/= c #\')
                                       (char/= c #\"))))
                     (setq buf (mkstr buf c)))
                   (cond ((and find-attri-value?
                               (char= c find-attri-value?))    ; quote(') or double quote(")
                          (push (delete find-attri-value?  buf) acc)
                          (initialize))
                         ((and find-attri-name?
                               (not find-attri-value?)
                               (or (char= c #\')
                                   (char= c #\")))
                          (setq buf ""
                                find-attri-value? c))
                         ((and find-tag?
                               (string-equal buf attri))
                          (setq find-attri-name? t))
                         ((and (not find-tag?)
                               (string-equal buf tag))
                          (setq buf ""
                                find-tag? t))
                         ((and find-tag?
                               (not find-attri-name?)
                               (char= c #\Space))
                          (setq buf ""))
                         ((or (char= c #\<)
                              (char= c #\>))
                          (initialize))))
                 (nreverse acc))))
    (fn nil)))
;; Demo{{{

;; aタグのhref属性全抽出
(get-attribute "a" "href" " <HEAD> <a href=\"Content-Type\" content=\"text/html\" charset=\"Shift_JIS\"> <a http-equiv=\"Content-Language\" content=\"ja\"> <a name=\"Generator\"  content=\"MSHTML 5.00.2314.1000\"> <a href\"Author\" content=\"2007.8.9 A.Sasaki\"> </HEAD>")
;; aタグのcontent属性全抽出
(get-attribute "a" "content" " <HEAD> <a href='Content-Type' content='text/html' charset=\"Shift_JIS\"> <a http-equiv=\"Content-Language\" content=\"ja\"> <a name=\"Generator\"  content=\"MSHTML 5.00.2314.1000\"> <a href\"Author\" content=\"2007.8.9 A.Sasaki\"> </HEAD>")
;; aタグのcharset属性全抽出
(get-attribute "a" "charset" " <HEAD> <a href=\"Content-Type\"  charset=\"Shift_JIS\"> </HEAD>")

;; }}}

(defexe get-attr ((-e --extension) -r (-q --quiet) --help)
  "e:extension:r;q;quiet;help"
  (let ((usage (usage :title "get-attr [OPTION]... [TAG] [ATTRIBUTE]"
                      :desc  '("Print ATTRIBUTE of TAG.")
                      :opts  '("-e, --extension" "target file's extension (default `html', `htm')"
                               "-r" "search file recursive"
                               "-q, --quiet" "never print headers giving file names")))
        (tag (first args))
        (attr (second args))
        (quiet (or -q --quiet))
        (extension (aif (or -e --extension)
                     (mksym it)
                     '(html htm))))
    (when (or --help
              errors
              (null tag)
              (null attr))
      (funcall usage))
    (mapfile (lambda (pathname)
               (unless quiet
                 (echo "==> " pathname " <=="))
               (mapcan #'echo
                       (get-attribute tag attr (list->string (read-from pathname)))))
             :extension extension
             :recursive -r)))

