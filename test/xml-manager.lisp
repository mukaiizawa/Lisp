
(require "xml-manager" *module-xml-manager*)
(require "test-utils" *module-test-utils*)

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
<span id="L01" class="LineNr"> 9 </span><span class="Comment">&quot; Comment</span>
<span id="L02" class="LineNr">10 </span><span class="Statement">highlight</span> <span class="Type">Comment</span>        <span class="Type">guifg</span>=<span class="Number">#a6f02e</span>
<span id="L03" class="LineNr">11 </span><span class="Comment">&quot; Constant</span>
<span id="L04" class="LineNr">12 </span><span class="Statement">highlight</span> <span class="Type">Constant</span>       <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span id="L05" class="LineNr">13 </span><span class="Statement">highlight</span> <span class="Type">String</span>         <span class="Type">guifg</span>=<span class="Number">#ffaaaa</span>
<span id="L06" class="LineNr">14 </span><span class="Statement">highlight</span> <span class="Type">Character</span>      <span class="Type">guifg</span>=<span class="Number">#db7093</span>
<span id="L07" class="LineNr">15 </span><span class="Statement">highlight</span> <span class="Type">Number</span>         <span class="Type">guifg</span>=<span class="Number">#fadd5a</span>
<span id="L08" class="LineNr">16 </span><span class="Statement">highlight</span> <span class="Type">Boolean</span>        <span class="Type">guifg</span>=<span class="Number">#87ceeb</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span id="L09" class="LineNr">17 </span><span class="Statement">highlight</span> <span class="Type">Float</span>          <span class="Type">guifg</span>=<span class="Number">#fadd5a</span>
<span id="L10" class="LineNr">18 </span><span class="Comment">&quot; Identifier</span>
<span id="L11" class="LineNr">19 </span><span class="Statement">highlight</span> <span class="Type">Identifier</span>     <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span id="L12" class="LineNr">20 </span><span class="Statement">highlight</span> <span class="Type">Function</span>       <span class="Type">guifg</span>=<span class="Number">#fad07a</span>
<span id="L13" class="LineNr">21 </span><span class="Comment">&quot; Statement</span>
<span id="L14" class="LineNr">22 </span><span class="Statement">highlight</span> <span class="Type">Statement</span>      <span class="Type">guifg</span>=<span class="Number">#cc7833</span>
<span id="L15" class="LineNr">23 </span><span class="Statement">highlight</span> <span class="Type">Exception</span>      <span class="Type">guifg</span>=<span class="Number">#f86060</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span id="L16" class="LineNr">24 </span><span class="Comment">&quot; Preproc</span>
<span id="L17" class="LineNr">25 </span><span class="Statement">highlight</span> <span class="Type">PreProc</span>        <span class="Type">guifg</span>=<span class="Number">#fad07a</span>
<span id="L18" class="LineNr">26 </span><span class="Comment">&quot; Type</span>
<span id="L19" class="LineNr">27 </span><span class="Statement">highlight</span> <span class="Type">Type</span>           <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span id="L20" class="LineNr">28 </span><span class="Statement">highlight</span> <span class="Type">Structure</span>      <span class="Type">guifg</span>=<span class="Number">#cc7833</span>
<span id="L21" class="LineNr">29 </span><span class="Comment">&quot; Special</span>
<span id="L22" class="LineNr">30 </span><span class="Statement">highlight</span> <span class="Type">Special</span>        <span class="Type">guifg</span>=<span class="Number">#87ceeb</span>
<span id="L23" class="LineNr">31 </span><span class="Statement">highlight</span> <span class="Type">Delimiter</span>      <span class="Type">guifg</span>=<span class="Number">#40e0db</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span id="L24" class="LineNr">32 </span><span class="Statement">highlight</span> <span class="Type">SpecialComment</span> <span class="Type">guifg</span>=<span class="Number">#a6f02e</span>
<span id="L25" class="LineNr">33 </span><span class="Comment">&quot; Underlined</span>
<span id="L26" class="LineNr">34 </span><span class="Statement">highlight</span> <span class="Type">Underlined</span>     <span class="Type">guifg</span>=<span class="Number">#00bfff</span>
<span id="L27" class="LineNr">35 </span><span class="Comment">&quot; Ignore</span>
<span id="L28" class="LineNr">36 </span><span class="Statement">highlight</span> <span class="Type">Ignore</span>         <span class="Type">guifg</span>=<span class="Number">#ff0000</span>
<span id="L29" class="LineNr">37 </span><span class="Comment">&quot; Error</span>
<span id="L30" class="LineNr">38 </span><span class="Statement">highlight</span> <span class="Type">Error</span>          <span class="Type">guifg</span>=<span class="Number">#66d9ef</span> <span class="Type">guibg</span>=<span class="Number">#202020</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
<span id="L31" class="LineNr">39 </span><span class="Comment">&quot; Todo</span>
<span id="L32" class="LineNr">40 </span><span class="Statement">highlight</span> <span class="Type">Todo</span>           <span class="Type">guifg</span>=<span class="Number">#66d9ef</span> <span class="Type">guibg</span>=<span class="Number">#202020</span> <span class="Type">gui</span>=<span class="PreProc">bold</span>
END)

;; }}}
;; sample-css {{{

(defun sample-css ()
#<< END
<style type="text/css">
pre { font-family: monospace; color: #ffffff; background-color: #303030; }
body { font-family: monospace; color: #ffffff; background-color: #303030; }
* { font-size: 1em; }
.Function { color: #fad07a; }
.Number { color: #fadd5a; }
.Type { color: #87ceeb; font-weight: bold; }
.Statement { color: #cc7833; font-weight: bold; }
.LineNr { color: #aaaaaa; }
.PreProc { color: #fad07a; }
.Delimiter { color: #40e0db; font-weight: bold; }
.Identifier { color: #87ceeb; }
.Comment { color: #a6f02e; }
.String { color: #ffaaaa; }
</style>
END)

;; }}}

(setq *with-format* t)

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

