
(require "stdlib" *module-stdlib*)

(defun string->char-list (str)
  (if (empty? str)
    (list #\Space)
    (concatenate 'list
                 (list (char str 0))
                 (string->char-list (subseq str 1)))))

(defun make-print-table ()
  (let ((table (make-hash-table :size 80)))
    ;; number {{{
    ;; 0 {{{

    (push (list "        "
                "   _|   "
                " _|  _| "
                " _|  _| "
                " _|  _| "
                "   _|   "
                "        "
                "        ")
          (gethash #\0 table))

    ;; }}}
    ;; 1 {{{

    (push (list "        "
                "   _|   "
                " _|_|   "
                "   _|   "
                "   _|   "
                "   _|   "
                "        "
                "        ")
          (gethash #\1 table))

    ;; }}}
    ;; 2 {{{

    (push (list "          "
                "   _|_|   "
                " _|    _| "
                "     _|   "
                "   _|     "
                " _|_|_|_| "
                "          "
                "          ")
          (gethash #\2 table))

    ;; }}}
    ;; 3 {{{

    (push (list "          "
                " _|_|_|   "
                "       _| "
                "   _|_|   "
                "       _| "
                " _|_|_|   "
                "          "
                "          ")
          (gethash #\3 table))

    ;; }}}
    ;; 4 {{{

    (push (list "          "
                " _|  _|   "
                " _|  _|   "
                " _|_|_|_| "
                "     _|   "
                "     _|   "
                "          "
                "          ")
          (gethash #\4 table))

    ;; }}}
    ;; 5 {{{

    (push (list "          "
                " _|_|_|_| "
                " _|       "
                " _|_|_|   "
                "       _| "
                " _|_|_|   "
                "          "
                "          ")
          (gethash #\5 table))

    ;; }}}
    ;; 6 {{{

    (push (list "          "
                "   _|_|_| "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                "   _|_|   "
                "          "
                "          ")
          (gethash #\6 table))

    ;; }}}
    ;; 7 {{{

    (push (list "          "
                " _|_|_|_| "
                "      _|  "
                "    _|    "
                "   _|     "
                "  _|      "
                "          "
                "          ")
          (gethash #\7 table))

    ;; }}}
    ;; 8 {{{

    (push (list "          "
                "   _|_|   "
                " _|    _| "
                "   _|_|   "
                " _|    _| "
                "   _|_|   "
                "          "
                "          ")
          (gethash #\8 table))

    ;; }}}
    ;; 9 {{{

    (push (list "          "
                "   _|_|   "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                " _|_|_|   "
                "          "
                "          ")
          (gethash #\9 table))

    ;; }}}
    ;; }}}
    ;; [a-z] {{{
    ;; a {{{
    (push (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          ")
          (gethash #\a table))

    ;; }}}
    ;; b {{{

    (push (list "          "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|_|_|   "
                "          "
                "          ")
          (gethash #\b table))

    ;; }}}
    ;; c {{{

    (push (list "          "
                "          "
                "   _|_|_| "
                " _|       "
                " _|       "
                "   _|_|_| "
                "          "
                "          ")
          (gethash #\c table))

    ;; }}}
    ;; d {{{

    (push (list "       _| "
                "       _| "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          ")
          (gethash #\d table))

    ;; }}}
    ;; e {{{

    (push (list "          "
                "          "
                "   _|_|   "
                " _|_|_|_| "
                " _|       "
                "   _|_|_| "
                "          "
                "          ")
          (gethash #\e table))

    ;; }}}
    ;; f {{{

    (push (list "          "
                "     _|_| "
                "   _|     "
                " _|_|_|_| "
                "   _|     "
                "   _|     "
                "          "
                "          ")
          (gethash #\f table))

    ;; }}}
    ;; g {{{

    (push (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "   _|_|   ")
          (gethash #\g table))

    ;; }}}
    ;; h {{{

    (push (list "          "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "          "
                "          ")
          (gethash #\h table))

    ;; }}}
    ;; i {{{

    (push (list "     "
                "  _| "
                "     "
                "  _| "
                "  _| "
                "  _| "
                "     "
                "     ")
          (gethash #\i table))

    ;; }}}
    ;; i {{{

    (push (list "      "
                "   _| "
                "      "
                "   _| "
                "   _| "
                "   _| "
                "  _|  "
                " _|   ")
          (gethash #\j table))

    ;; }}}
    ;; k {{{

    (push (list "          "
                " _|       "
                " _|  _|   "
                " _|_|     "
                " _|  _|   "
                " _|    _| "
                "          "
                "          ")
          (gethash #\k table))

    ;; }}}
    ;; l {{{

    (push (list "     "
                "  _| "
                "  _| "
                "  _| "
                "  _| "
                "  _| "
                "     "
                "     ")
          (gethash #\l table))

    ;; }}}
    ;; m {{{

    (push (list "              "
                "              "
                " _|_|_| _|_|  "
                " _|   _|   _| "
                " _|   _|   _| "
                " _|   _|   _| "
                "              "
                "              ")
          (gethash #\m table))

    ;; }}}
    ;; n {{{

    (push (list "          "
                "          "
                "  _|_|_|  "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "          "
                "          ")
          (gethash #\n table))

    ;; }}}
    ;; o {{{

    (push (list "          "
                "          "
                "   _|_|   "
                " _|    _| "
                " _|    _| "
                "   _|_|   "
                "          "
                "          ")
          (gethash #\o table))

    ;; }}}
    ;; p {{{

    (push (list "          "
                "          "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|_|_|   "
                " _|       "
                " _|       ")
          (gethash #\p table))

    ;; }}}
    ;; p {{{

    (push (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "       _| ")
          (gethash #\q table))

    ;; }}}
    ;; r {{{

    (push (list "          "
                "          "
                " _|  _|_| "
                " _|_|     "
                " _|       "
                " _|       "
                "          "
                "          ")
          (gethash #\r table))

    ;; }}}
    ;; s {{{

    (push (list "          "
                "          "
                "   _|_|_| "
                " _|_|     "
                "     _|_| "
                " _|_|_|   "
                "          "
                "          ")
          (gethash #\s table))

    ;; }}}
    ;; t {{{

    (push (list "          "
                "   _|     "
                " _|_|_|_| "
                "   _|     "
                "   _|     "
                "     _|_| "
                "          "
                "          ")
          (gethash #\t table))

    ;; }}}
    ;; u {{{

    (push (list "          "
                "          "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          ")
          (gethash #\u table))

    ;; }}}
    ;; v {{{

    (push (list "               "
                "               "
                "  _|        _| "
                "   _|      _|  "
                "     _|  _|    "
                "       _|      "
                "               "
                "               ")
          (gethash #\v table))

    ;; }}}
    ;; w {{{

    (push (list "                    "
                "                    "
                " _|      _|      _| "
                "  _|     _|     _|  "
                "   _|  _|  _|  _|   "
                "     _|      _|     "
                "                    "
                "                    ")
          (gethash #\w table))

    ;; }}}
    ;; x {{{

    (push (list "           "
                "           "
                "  _|    _| "
                "    _|_|   "
                "    _|_|   "
                "  _|    _| "
                "           "
                "           ") (gethash #\x table))

    ;; }}}
    ;; y {{{

    (push (list "          "
                "          "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "   _|_|   ")
          (gethash #\y table))

    ;; }}}
    ;; z {{{

    (push (list "           "
                "           "
                "  _|_|_|_| "
                "      _|   "
                "    _|     "
                "  _|_|_|_| "
                "           "
                "           ")
          (gethash #\z table))

    ;; }}}
    ;; }}}
    ;; [A-Z] {{{
    ;; A {{{

    (push  (list "          "
                 "   _|_|   "
                 " _|    _| "
                 " _|_|_|_| "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          ")
           (gethash #\A table))

    ;; }}}
    ;; B {{{

    (push  (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 "          "
                 "          ")
           (gethash #\B table))

    ;; }}}
    ;; C {{{

    (push  (list "          "
                 "   _|_|_| "
                 " _|       "
                 " _|       "
                 " _|       "
                 "   _|_|_| "
                 "          "
                 "          ")
           (gethash #\C table))

    ;; }}}
    ;; D {{{

    (push  (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 " _|_|_|   "
                 "          "
                 "          ")
           (gethash #\D table))

    ;; }}}
    ;; E {{{

    (push  (list "          "
                 " _|_|_|_| "
                 " _|       "
                 " _|_|_|   "
                 " _|       "
                 " _|_|_|_| "
                 "          "
                 "          ")
           (gethash #\E table))

    ;; }}}
    ;; F {{{

    (push  (list "          "
                 " _|_|_|_| "
                 " _|       "
                 " _|_|_|   "
                 " _|       "
                 " _|       "
                 "          "
                 "          ")
           (gethash #\F table))

    ;; }}}
    ;; G {{{

    (push  (list "          "
                 "   _|_|_| "
                 " _|       "
                 " _|  _|_| "
                 " _|    _| "
                 "   _|_|_| "
                 "          "
                 "          ")
           (gethash #\G table))

    ;; }}}
    ;; H {{{

    (push  (list "          "
                 " _|    _| "
                 " _|    _| "
                 " _|_|_|_| "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          ")
           (gethash #\H table))

    ;; }}}
    ;; I {{{

    (push  (list "        "
                 " _|_|_| "
                 "   _|   "
                 "   _|   "
                 "   _|   "
                 " _|_|_| "
                 "        "
                 "        ")
           (gethash #\I table))

    ;; }}}
    ;; J {{{

    (push  (list "          "
                 "       _| "
                 "       _| "
                 "       _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          ")
           (gethash #\J table))

    ;; }}}
    ;; K {{{

    (push  (list "          "
                 " _|    _| "
                 " _|  _|   "
                 " _|_|     "
                 " _|  _|   "
                 " _|    _| "
                 "          "
                 "          ")
           (gethash #\K table))

    ;; }}}
    ;; L {{{

    (push  (list "          "
                 " _|       "
                 " _|       "
                 " _|       "
                 " _|       "
                 " _|_|_|_| "
                 "          "
                 "          ")
           (gethash #\L table))

    ;; }}}
    ;; M {{{

    (push  (list "           "
                 "_|      _| "
                 "_|_|  _|_| "
                 "_|  _|  _| "
                 "_|      _| "
                 "_|      _| "
                 "           "
                 "           ")
           (gethash #\M table))

    ;; }}}
    ;; N {{{

    (push  (list "             "
                 "  _|      _| "
                 "  _|_|    _| "
                 "  _|  _|  _| "
                 "  _|    _|_| "
                 "  _|      _| "
                 "             "
                 "             ")
           (gethash #\N table))

    ;; }}}
    ;; O {{{

    (push  (list "          "
                 "   _|_|   "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          ")
           (gethash #\O table))

    ;; }}}
    ;; P {{{

    (push  (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|       "
                 " _|       "
                 "          "
                 "          ")
           (gethash #\P table))

    ;; }}}
    ;; Q {{{

    (push  (list "            "
                 "   _|_|     "
                 " _|    _|   "
                 " _|  _|_|   "
                 " _|    _|   "
                 "   _|_|  _| "
                 "            "
                 "            ")
           (gethash #\Q table))

    ;; }}}
    ;; R {{{

    (push  (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          ")
           (gethash #\R table))

    ;; }}}
    ;; S {{{

    (push  (list "          "
                 "   _|_|_| "
                 " _|       "
                 "   _|_|   "
                 "       _| "
                 " _|_|_|   "
                 "          "
                 "          ")
           (gethash #\S table))

    ;; }}}
    ;; T {{{

    (push  (list "            "
                 " _|_|_|_|_| "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "            "
                 "            ")
           (gethash #\T table))

    ;; }}}
    ;; U {{{

    (push  (list "          "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          ")
           (gethash #\U table))

    ;; }}}
    ;; V {{{

    (push  (list "               "
                 " _|         _| "
                 "  _|       _|  "
                 "   _|     _|   "
                 "     _|  _|    "
                 "       _|      "
                 "               "
                 "               ")
           (gethash #\V table))

    ;; }}}
    ;; W {{{

    (push  (list "                "
                 " _|          _| "
                 " _|          _| "
                 " _|    _|    _| "
                 "   _|  _|  _|   "
                 "     _|  _|     "
                 "                "
                 "                ")
           (gethash #\W table))

    ;; }}}
    ;; X {{{

    (push  (list "            "
                 " _|      _| "
                 "   _|  _|   "
                 "     _|     "
                 "   _|  _|   "
                 " _|      _| "
                 "            "
                 "            ")
           (gethash #\X table))

    ;; }}}
    ;; Y {{{

    (push  (list "            "
                 " _|      _| "
                 "   _|  _|   "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "            "
                 "            ")
           (gethash #\Y table))

    ;; }}}
    ;; Z {{{

    (push  (list "            "
                 " _|_|_|_|_| "
                 "       _|   "
                 "     _|     "
                 "   _|       "
                 " _|_|_|_|_| "
                 "            "
                 "            ")
           (gethash #\Z table))

    ;; }}}
    ;; }}}
    ;; etc {{{
    ;; . {{{

    (push (list "    "
                "    "
                "    "
                "    "
                "    "
                " _| "
                "    "
                "    ")
          (gethash #\. table))

    ;; }}}
    ;; ? {{{

    (push (list "        "
                " _|_|   "
                "     _| "
                " _|_|   "
                "        "
                " _|     "
                "        "
                "        ")
          (gethash #\? table))

    ;; }}}
    ;; ! {{{

    (push (list "    "
                " _| "
                " _| "
                " _| "
                "    "
                " _| "
                "    "
                "    ")
          (gethash #\! table))

    ;; }}}
    ;; - {{{

    (push (list "            "
                "            "
                "            "
                " _|_|_|_|_| "
                "            "
                "            "
                "            "
                "            ")
          (gethash #\- table))

    ;; }}}
    ;; #\Space  {{{

    (push (list "    "
                "    "
                "    "
                "    "
                "    "
                "    "
                "    "
                "    ")
          (gethash #\Space table))

    ;; }}}
    ;; }}}
    table))

(defparameter *print-table* (make-print-table))

(defun char-list->aa-list (char-list &key (background nil))
  (let ((result nil))
    (dotimes (i 8)
      (push (apply #'mkstr (mapcar (lambda (x)
                                     (nth i (car (gethash x *print-table*))))
                                   char-list))
            result)
      (if background 
        (push (replstr " " background (pop result)) result)))
    (nreverse result)))

(defexe aprint ((-s --surround-with) (-g --bg-char) --help)
  "s:surround-with:g:bg-char:help"
  (let* ((usage (usage :title "aprint"
                       :desc  "Text to ASCII Art Generator"
                       :opts '("-s, --surround-with=" "surround with a character."
                               "-g, --bg-char=" "background character.")))
         (surround-char (or -s --surround-with))
         (bg-char (or -g --bg-char))
         (aa-list (char-list->aa-list (string->char-list (list->string args)) :background bg-char))
         (surround-str (if surround-char (make-sequence 'string (length (car aa-list)) :initial-element (char surround-char 0)))))
    (if (or --help
            errors
            (> (length surround-char) 1))
      (funcall usage)
      (surround (echo surround-str)
        (dolist (i aa-list)
          (echo i))))))

