マークダウンリーダー

# 概要
独自拡張したマークダウン記法を解釈し構文木に変換する。

# 書式
拡張マークダウンの書式を以下に示す。
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
            <table_header>
            <table_separator>
            <table_body>
            <table_separator>
    <table_separator> ::= '--' <eol>
    <table_line> ::= <string> [<tab> <string>] ... <eol>
    <table_header> ::= <table_line>
    <table_body> ::= <table_line> <table_line> ...
    <title> -- この文書のタイトルを表す文字列
    <eol> -- 改行文字
    <string> -- 文字の列

## 見出し
'#'から始まる行は見出しと見做される。
    # header1
    ## header2
    ### header3
    #### header4
    ##### header5
    ###### header6

連続する#の数が見出しレベルに対応する。
# header1
## header2
### header3
#### header4
##### header5
###### header6

## 段落
行末までの文字の列は段落と見做される。
    paragraf...
paragraf...

## 整形済みテキスト
半角スペース4つから始まる行は整形済みテキストと見做す。
    preformatted

整形済みのテキストはフォーマッタによって整形されない。
    (defun make-adder (n)
      (lambda (x) (+ x n)))

## 引用
'>'から始まる行は引用文と見做す。引用の引用を表す場合は'>'をネストさせる。
    > quotation1
    > quotation2
    >> quotation of quotation1
    >> quotation of quotation2
> quotation1
> quotation2
>> quotation of quotation1
>> quotation of quotation2

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
    header1	header2	header3
    --
    body1-1	body1-2	body1-3
    body2-1	body2-2	body2-3
    body3-1	body3-2	body3-3
    --

--
header1	header2	header3
--
body1-1	body1-2	body1-3
body2-1	body2-2	body2-3
body3-1	body3-2	body3-3
--
