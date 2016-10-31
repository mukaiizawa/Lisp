
(require :xml-manager *module-xml-manager*)
(require :test-utils *module-test-utils*)

(defmacro test-xml->DSL (str)
  `(with-input-from-string (in ,str)
     (xml->DSL in)))

(defmacro with-escape (str)
  (with-string-ahead-reader (reader str)
    (get-buf (read-if (lambda (c) c) reader))))

(setq *with-format* nil)

(test-all
  (simple-01
   (test-xml->DSL"<a></a>")
   "(:a ())")
  (simple-02
   (test-xml->DSL"<a>a</a>")
   (with-escape "(:a ()\\n\"a\")"))
  (simple-02
   (test-xml->DSL"<a>a<b>b</b>a</a>")
   (with-escape "(:a ()\\n\"a\"\\n(:b ()\\n\"b\")\\n\"a\")"))
  (attribute-01
   (test-xml->DSL"<a a='b'></a>")
   (with-escape "(:a ((a \"b\")))"))
  (attribute-02
   (test-xml->DSL"<a a='b'></a>")
   (with-escape "(:a ((a \"b\")))"))
  (single-attribute
   (test-xml->DSL"<a a='b' c></a>")
   (with-escape "(:a ((a \"b\") (c)))"))
  (single-tag
   (test-xml->DSL"<hr><br>")
   (with-escape "(:hr ())(:br ())")))

;; sample-source {{{

(defun sample-source ()
#<< END
<span class="Statement">set</span> <span class="PreProc">background</span>=dark
<span class="Statement">hi</span> <span class="Statement">clear</span>
<span class="Statement">if</span> <span class="Function">exists</span><span class="Delimiter">(</span><span class="String">&quot;syntax_on&quot;</span><span class="Delimiter">)</span>
  <span class="Statement">syntax</span> <span class="Type">reset</span>
<span class="Statement">endif</span>
<span class="Statement">let</span> <span class="Identifier">g:colors_name</span> <span class="Statement">=</span> <span class="String">&quot;shin-dark&quot;</span>

<span class="Comment">&quot; :help group-name &quot;{{{</span>
<span class="Comment">&quot; Comment</span>
<span class="Statement">highlight</span> <span class="Type">Comment</span>        <span class="Type">guifg</span>=<span class="Number">#a6f02e</span>
<span class="Comment">&quot; Constant</span>
<span class="Statement">highlight</span> <span class="Type">Constant</span>       <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span class="Statement">highlight</span> <span class="Type">String</span>         <span class="Type">guifg</span>=<span class="Number">#ffaaaa</span>
<span class="Statement">highlight</span> <span class="Type">Character</span>      <span class="Type">guifg</span>=<span class="Number">#db7093</span>
<span class="Statement">highlight</span> <span class="Type">Number</span>         <span class="Type">guifg</span>=<span class="Number">#fadd5a</span>
<span class="Statement">highlight</span> <span class="Type">Boolean</span>        <span class="Type">guifg</span>=<span class="Number">#87ceeb</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span class="Statement">highlight</span> <span class="Type">Float</span>          <span class="Type">guifg</span>=<span class="Number">#fadd5a</span>
<span class="Comment">&quot; Identifier</span>
<span class="Statement">highlight</span> <span class="Type">Identifier</span>     <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span class="Statement">highlight</span> <span class="Type">Function</span>       <span class="Type">guifg</span>=<span class="Number">#fad07a</span>
<span class="Comment">&quot; Statement</span>
<span class="Statement">highlight</span> <span class="Type">Statement</span>      <span class="Type">guifg</span>=<span class="Number">#cc7833</span>
<span class="Statement">highlight</span> <span class="Type">Exception</span>      <span class="Type">guifg</span>=<span class="Number">#f86060</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span class="Comment">&quot; Preproc</span>
<span class="Statement">highlight</span> <span class="Type">PreProc</span>        <span class="Type">guifg</span>=<span class="Number">#fad07a</span>
<span class="Comment">&quot; Type</span>
<span class="Statement">highlight</span> <span class="Type">Type</span>           <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span class="Statement">highlight</span> <span class="Type">Structure</span>      <span class="Type">guifg</span>=<span class="Number">#cc7833</span>
<span class="Comment">&quot; Special</span>
<span class="Statement">highlight</span> <span class="Type">Special</span>        <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span class="Statement">highlight</span> <span class="Type">Delimiter</span>      <span class="Type">guifg</span>=<span class="Number">#40e0db</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span class="Statement">highlight</span> <span class="Type">SpecialComment</span> <span class="Type">guifg</span>=<span class="Number">#a6f02e</span>
<span class="Comment">&quot; Underlined</span>
<span class="Statement">highlight</span> <span class="Type">Underlined</span>     <span class="Type">guifg</span>=<span class="Number">#fad07a</span>
<span class="Comment">&quot; Ignore</span>
<span class="Statement">highlight</span> <span class="Type">Ignore</span>         <span class="Type">guifg</span>=<span class="Number">#ff0000</span>
<span class="Comment">&quot; Error</span>
<span class="Statement">highlight</span> <span class="Type">Error</span>          <span class="Type">guifg</span>=<span class="Number">#66d9ef</span> <span class="Type">guibg</span>=<span class="Number">#202020</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span class="Comment">&quot; Todo</span>
<span class="Statement">highlight</span> <span class="Type">Todo</span>           <span class="Type">guifg</span>=<span class="Number">#66d9ef</span> <span class="Type">guibg</span>=<span class="Number">#202020</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span class="Comment">&quot; }}}</span>

END)

;; }}}
;; sample-css {{{

(defun sample-css ()
#<< END
<style type="text/css">
pre { font-family: monospace; color: #ffffff; background-color: #262626; }
body { font-family: monospace; color: #ffffff; background-color: #262626; }
* { font-size: 1em; }
.PreProc { color: #ffd787; }
.Delimiter { color: #00ffff; font-weight: bold; }
.Comment { color: #87ff87; }
.Identifier { color: #87ffff; }
.Folded { color: #808080; background-color: #3a3a3a; padding-bottom: 1px; }
.Number { color: #ffffaf; }
.Type { color: #87ffff; }
.String { color: #ffafaf; }
.Function { color: #ffd787; }
.Statement { color: #d78700; }
</style>
END)

;; }}}

(write-to!
  (DSL->xml
    (:!DOCTYPE "html")
    (:html ((lang "ja"))
      (:head 
        (:meta ((charset "utf-8")))
        (:style (import-xml (sample-css)))
        (:title "xml-manager"))
      (:body
        (:!-- "comment node")
        (:hr)
        (:table ((border 0) (cellpadding 10))
          (loop for i from 0 to 3 collect
                (:tr ((align "right"))
                  (loop for j from 0 to 3 collect
                        (:td ((bgcolor (if (oddp (1+ j)) "#FF0000" "#FF00FF")))
                          (format nil "~@R" (+ (* 5 i) (1+ j))))))))
        (:hr)
        (to-pre-code (sample-source))
        )))
  "xml-manager.html")

