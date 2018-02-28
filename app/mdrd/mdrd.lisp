; markdown reader

#|
マークダウンリーダー

# 概要
独自拡張したマークダウン記法を解釈し構文木に変換する。

# 書式
    <markdown> ::= <title> <statement> ...
    <statement> ::= {
            <paragraph>
            | <preformatted_text_block>
            | <quote_block>
            | <list_block>
            | <table>
        }
    <header> ::= { # | ## | ### | #### | ##### | ###### } ' ' <string> <eol>
    <preformatted_text_block> ::= <preformatted_text> ...
    <preformatted_text> ::= '    ' <string> <eol>
    <quote_block> ::= <quote> ...
    <quote> ::= '>' ... ' ' <string> <eol>
    <list_block> ::= { <ordered_list> | <unordered_list> } ...
    <ordered_list> ::= '1.' ... ' ' <string> <eol>
    <unordered_list> ::= '-' ... ' ' <string> <eol>
    <table> ::= <table_separator>
            [<table_header>]
            <table_body>
            <table_separator>
    <table_separator> ::= '--' <eol>
    <table_line> ::= <string> [<tab> <string>] ... <eol>
    <table_header> ::= <table_line> <table_separator>
    <table_body> ::= <table_line> <table_line> ...
    <title> -- この文書のタイトルを表す文字列
    <eol> -- 改行文字
    <string> -- 文字の列

## 見出し
#から始まる行は見出しと見做される。
    # header1
    ## header2
    ### header3
    #### header4
    ##### header5
    ###### header6

連続する#の数が見出しレベルに対応する。

## 段落
行末までの文字の列は段落と見做される。

## 整形済みテキスト
半角スペース4つから始まる行は整形済みテキストと見做す。

整形済みのテキストはフォーマッタによって整形されない。

## 引用
>から始まる行は引用文と見做す。
    > quotation

ネストすることにより引用の引用を表すことができる。
    >> quotation of quotation

## リスト
リストは順序の有無により二種類存在する。
--
順序	開始文字
--
順序あり	1.
順序無し	-
--

それぞれ、開始文字を重ねることにより、ネストしたリストを表現することができる。また、リスト内で他方のリストを記述することもできる。
    - list
    -- nested list
    - list
    1. ordered list
    1. ordered list
    - list

## 表
'--'で区切られたセクションは表と見做される。表は省略可能なヘッダ―部とボディー部に分かれる。

次のようにタブ区切りの列として記述される。
--
header1	header2
--
body1-1	body1-2
body2-1	body2-2
--

|#

(require :ahead-reader *module-ahead-reader*)
(require :xml-manager *module-xml-manager*)

(defparameter *outline* nil)

(defun trim-left (s)
  (if (char= (char s 0) #\space)
    (trim-left (subseq s 1))
    s))

; (defun parse-outline (outline)
;   (labels
;     ((rec (outline)
;           `(:ul ,@(mapcar (lambda (x) `(:ol ,x)) outline)))
; ; #o(parse-outline '((1 a) (2 b) (3 c)))

(defmethod read-blank ((ar ahead-reader))
  (read-if (lambda (c) (char= c #\newline)) ar :cache nil)
  ar)

(defmethod get-line ((ar ahead-reader))
  (get-buf (xread-line ar)))

(defmethod parse-header ((ar ahead-reader))
  (let ((line (get-line ar)) (level 0))
    (while (char= (char line 0) #\#)
      (setq line (subseq line 1))
      (incf level))
    (setq line (trim-left line))
    (push (list level line) *outline*)
    (ecase level
      ((1) `(:h1 ,line))
      ((2) `(:h2 ,line))
      ((3) `(:h3 ,line))
      ((4) `(:h4 ,line))
      ((5) `(:h5 ,line))
      ((6) `(:h6 ,line)))))

(defmethod parse-quote-block ((ar ahead-reader))
  (parse-paragraph ar))

(defmethod parse-paragraph ((ar ahead-reader))
  `(:p ,(get-line ar)))

(defmethod preformatted_text? ((ar ahead-reader) &optional (col 1))
  (or (> col 4)
      (and (char= (get-next ar col) #\space)
           (preformatted_text? ar (1+ col)))))

(defmethod parse-preformatted_text_block ((ar ahead-reader))
  (let (line)
    (while (preformatted_text? ar)
      (push (subseq (get-line ar) 4) line))
    `(:pre ,@(nreverse line))))

(defmethod parse-statement ((ar ahead-reader))
  (cond ((char= (get-next (read-blank ar)) #\#) (parse-header ar))
        ((char= (get-next ar) #\>) (parse-quote-block ar))
        ((preformatted_text? ar) (parse-preformatted_text_block ar))
        (t (parse-paragraph ar))))

(defmethod parse-title ((ar ahead-reader))
  (get-line ar))

(defmethod parse-markdown((ar ahead-reader))
  (let ((title (parse-title ar))
        (body nil))
    (while (not (reach-eof? (read-blank ar)))
      (push (parse-statement ar) body))
    `((:!DOCTYPE "html")
      (:html ((lang "ja"))
        (:head 
          (:meta ((charset "utf-8")))
          (:title ,title))
        (:body
          ; ,(parse-outline *outline*)
          ,@(nreverse body))))))

(defun read-markdown (stream)
  (with-ahead-reader (ar stream)
    (parse-markdown ar)))

(setq *with-format* t)

; {{{
(defparameter *test-code*
"マークダウンリーダー

# 概要
独自拡張したマークダウン記法を解釈し構文木に変換する。

# 書式
    <markdown> ::= <title> <block> ...
    <block> ::= <header> { <block> | <statement> } ...
    <statement> ::= {
            <paragraph>
            | <preformatted_text_block>
            | <quote_block>
            | <list_block>
            | <table>
        }
    <header> ::= { # | ## | ### | #### | ##### | ###### } ' ' <string> <eol>
    <preformatted_text_block> ::= <preformatted_text> ...
    <preformatted_text> ::= '    ' <string> <eol>
    <quote_block> ::= <quote> ...
    <quote> ::= '>' ... ' ' <string> <eol>
    <list_block> ::= { <ordered_list> | <unordered_list> } ...
    <ordered_list> ::= '1.' ... ' ' <string> <eol>
    <unordered_list> ::= '-' ... ' ' <string> <eol>
    <table> ::= <table_separator>
            [<table_header>]
            <table_body>
            <table_separator>
    <table_separator> ::= '--' <eol>
    <table_line> ::= <string> [<tab> <string>] ... <eol>
    <table_header> ::= <table_line> <table_separator>
    <table_body> ::= <table_line> <table_line> ...
    <title> -- この文書のタイトルを表す文字列
    <eol> -- 改行文字
    <string> -- 文字の列

## 見出し
#から始まる行は見出しと見做される。
    # header1
    ## header2
    ### header3
    #### header4
    ##### header5
    ###### header6

連続する#の数が見出しレベルに対応する。見出しレベルが連続でない場合はエラーと見做す。

## 段落
行末までの文字の列は段落と見做される。

## 整形済みテキスト
半角スペース4つから始まる行は整形済みテキストと見做す。

整形済みのテキストはフォーマッタによって整形されない。

## 引用
>から始まる行は引用文と見做す。
    > quotation

ネストすることにより引用の引用を表すことができる。
    >> quotation of quotation

## リスト
リストは順序の有無により二種類存在する。
--
順序	開始文字
--
順序あり	1.
順序無し	-
--

それぞれ、開始文字を重ねることにより、ネストしたリストを表現することができる。また、リスト内で他方のリストを記述することもできる。
    - list
    -- nested list
    - list
    1. ordered list
    1. ordered list
    - list

## 表
'--'で区切られたセクションは表と見做される。表は省略可能なヘッダ―部とボディー部に分かれる。

次のようにタブ区切りの列として記述される。
--
header1	header2
--
body1-1	body1-2
body2-1	body2-2
--

")
; }}}

(with-open-file (out "test.html" :direction :output :if-exists :supersede)
  (princ
    (princ
      (with-input-from-string (in *test-code*)
        (DSL->xml (mapcar #'eval (read-markdown in))))
      out)))
