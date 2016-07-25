(load "../../lib/stdlib")


(defun string->char-list (str)
  (if (emptyp str)
    (list #\Space)
    (concatenate 'list
                 (list (char str 0))
                 (string->char-list (subseq str 1)))))


(let ((table (mkhash :size 80)))
  ;; number {{{
  (push (list "        "
              "   _|   "
              " _|  _| "
              " _|  _| "
              " _|  _| "
              "   _|   "
              "        "
              "        ")
        (gethash #\0 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "        "
              "   _|   "
              " _|_|   "
              "   _|   "
              "   _|   "
              "   _|   "
              "        "
              "        ")
        (gethash #\1 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "   _|_|   "
              " _|    _| "
              "     _|   "
              "   _|     "
              " _|_|_|_| "
              "          "
              "          ")
        (gethash #\2 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|_|_|   "
              "       _| "
              "   _|_|   "
              "       _| "
              " _|_|_|   "
              "          "
              "          ")
        (gethash #\3 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|  _|   "
              " _|  _|   "
              " _|_|_|_| "
              "     _|   "
              "     _|   "
              "          "
              "          ")
        (gethash #\4 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|_|_|_| "
              " _|       "
              " _|_|_|   "
              "       _| "
              " _|_|_|   "
              "          "
              "          ")
        (gethash #\5 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "   _|_|_| "
              " _|       "
              " _|_|_|   "
              " _|    _| "
              "   _|_|   "
              "          "
              "          ")
        (gethash #\6 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|_|_|_| "
              "      _|  "
              "    _|    "
              "   _|     "
              "  _|      "
              "          "
              "          ")
        (gethash #\7 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "   _|_|   "
              " _|    _| "
              "   _|_|   "
              " _|    _| "
              "   _|_|   "
              "          "
              "          ")
        (gethash #\8 table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; char-upcase {{{
  (push (list "          "
              "          "
              "   _|_|_| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "          "
              "          ")
        (gethash #\a table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|       "
              " _|_|_|   "
              " _|    _| "
              " _|    _| "
              " _|_|_|   "
              "          "
              "          ")
        (gethash #\b table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|_| "
              " _|       "
              " _|       "
              "   _|_|_| "
              "          "
              "          ")
        (gethash #\c table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "       _| "
              "       _| "
              "   _|_|_| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "          "
              "          ")
        (gethash #\d table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|   "
              " _|_|_|_| "
              " _|       "
              "   _|_|_| "
              "          "
              "          ")
        (gethash #\e table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "     _|_| "
              "   _|     "
              " _|_|_|_| "
              "   _|     "
              "   _|     "
              "          "
              "          ")
        (gethash #\f table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|_| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "       _| "
              "   _|_|   ")
        (gethash #\g table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|       "
              " _|_|_|   "
              " _|    _| "
              " _|    _| "
              " _|    _| "
              "          "
              "          ")
        (gethash #\h table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "     "
              "  _| "
              "     "
              "  _| "
              "  _| "
              "  _| "
              "     "
              "     ")
        (gethash #\i table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "      "
              "   _| "
              "      "
              "   _| "
              "   _| "
              "   _| "
              "  _|  "
              " _|   ")
        (gethash #\j table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              " _|       "
              " _|  _|   "
              " _|_|     "
              " _|  _|   "
              " _|    _| "
              "          "
              "          ")
        (gethash #\k table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "     "
              "  _| "
              "  _| "
              "  _| "
              "  _| "
              "  _| "
              "     "
              "     ")
        (gethash #\l table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "              "
              "              "
              " _|_|_| _|_|  "
              " _|   _|   _| "
              " _|   _|   _| "
              " _|   _|   _| "
              "              "
              "              ")
        (gethash #\m table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "  _|_|_|  "
              " _|    _| "
              " _|    _| "
              " _|    _| "
              "          "
              "          ")
        (gethash #\n table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|   "
              " _|    _| "
              " _|    _| "
              "   _|_|   "
              "          "
              "          ")
        (gethash #\o table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              " _|_|_|   "
              " _|    _| "
              " _|    _| "
              " _|_|_|   "
              " _|       "
              " _|       ")
        (gethash #\p table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|_| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "       _| "
              "       _| ")
        (gethash #\q table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              " _|  _|_| "
              " _|_|     "
              " _|       "
              " _|       "
              "          "
              "          ")
        (gethash #\r table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              "   _|_|_| "
              " _|_|     "
              "     _|_| "
              " _|_|_|   "
              "          "
              "          ")
        (gethash #\s table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "   _|     "
              " _|_|_|_| "
              "   _|     "
              "   _|     "
              "     _|_| "
              "          "
              "          ")
        (gethash #\t table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              " _|    _| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "          "
              "          ")
        (gethash #\u table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "               "
              "               "
              "  _|        _| "
              "   _|      _|  "
              "     _|  _|    "
              "       _|      "
              "               "
              "               ")
        (gethash #\v table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "                    "
              "                    "
              " _|      _|      _| "
              "  _|     _|     _|  "
              "   _|  _|  _|  _|   "
              "     _|      _|     "
              "                    "
              "                    ")
        (gethash #\w table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "           "
              "           "
              "  _|    _| "
              "    _|_|   "
              "    _|_|   "
              "  _|    _| "
              "           "
              "           ") (gethash #\x table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "          "
              "          "
              " _|    _| "
              " _|    _| "
              " _|    _| "
              "   _|_|_| "
              "       _| "
              "   _|_|   ")
        (gethash #\y table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; char-downcase {{{
  (push  (list "          "
               "   _|_|   "
               " _|    _| "
               " _|_|_|_| "
               " _|    _| "
               " _|    _| "
               "          "
               "          ")
         (gethash #\A table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|   "
               " _|    _| "
               " _|_|_|   "
               " _|    _| "
               " _|_|_|   "
               "          "
               "          ")
         (gethash #\B table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               "   _|_|_| "
               " _|       "
               " _|       "
               " _|       "
               "   _|_|_| "
               "          "
               "          ")
         (gethash #\C table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|   "
               " _|    _| "
               " _|    _| "
               " _|    _| "
               " _|_|_|   "
               "          "
               "          ")
         (gethash #\D table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|_| "
               " _|       "
               " _|_|_|   "
               " _|       "
               " _|_|_|_| "
               "          "
               "          ")
         (gethash #\E table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|_| "
               " _|       "
               " _|_|_|   "
               " _|       "
               " _|       "
               "          "
               "          ")
         (gethash #\F table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               "   _|_|_| "
               " _|       "
               " _|  _|_| "
               " _|    _| "
               "   _|_|_| "
               "          "
               "          ")
         (gethash #\G table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|    _| "
               " _|    _| "
               " _|_|_|_| "
               " _|    _| "
               " _|    _| "
               "          "
               "          ")
         (gethash #\H table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "        "
               " _|_|_| "
               "   _|   "
               "   _|   "
               "   _|   "
               " _|_|_| "
               "        "
               "        ")
         (gethash #\I table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               "       _| "
               "       _| "
               "       _| "
               " _|    _| "
               "   _|_|   "
               "          "
               "          ")
         (gethash #\J table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|    _| "
               " _|  _|   "
               " _|_|     "
               " _|  _|   "
               " _|    _| "
               "          "
               "          ")
         (gethash #\K table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|       "
               " _|       "
               " _|       "
               " _|       "
               " _|_|_|_| "
               "          "
               "          ")
         (gethash #\L table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "           "
               "_|      _| "
               "_|_|  _|_| "
               "_|  _|  _| "
               "_|      _| "
               "_|      _| "
               "           "
               "           ")
         (gethash #\M table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "             "
               "  _|      _| "
               "  _|_|    _| "
               "  _|  _|  _| "
               "  _|    _|_| "
               "  _|      _| "
               "             "
               "             ")
         (gethash #\N table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               "   _|_|   "
               " _|    _| "
               " _|    _| "
               " _|    _| "
               "   _|_|   "
               "          "
               "          ")
         (gethash #\O table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|   "
               " _|    _| "
               " _|_|_|   "
               " _|       "
               " _|       "
               "          "
               "          ")
         (gethash #\P table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "            "
               "   _|_|     "
               " _|    _|   "
               " _|  _|_|   "
               " _|    _|   "
               "   _|_|  _| "
               "            "
               "            ")
         (gethash #\Q table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|_|_|   "
               " _|    _| "
               " _|_|_|   "
               " _|    _| "
               " _|    _| "
               "          "
               "          ")
         (gethash #\R table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               "   _|_|_| "
               " _|       "
               "   _|_|   "
               "       _| "
               " _|_|_|   "
               "          "
               "          ")
         (gethash #\S table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "            "
               " _|_|_|_|_| "
               "     _|     "
               "     _|     "
               "     _|     "
               "     _|     "
               "            "
               "            ")
         (gethash #\T table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "          "
               " _|    _| "
               " _|    _| "
               " _|    _| "
               " _|    _| "
               "   _|_|   "
               "          "
               "          ")
         (gethash #\U table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "               "
               " _|         _| "
               "  _|       _|  "
               "   _|     _|   "
               "     _|  _|    "
               "       _|      "
               "               "
               "               ")
         (gethash #\V table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "                "
               " _|          _| "
               " _|          _| "
               " _|    _|    _| "
               "   _|  _|  _|   "
               "     _|  _|     "
               "                "
               "                ")
         (gethash #\W table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "            "
               " _|      _| "
               "   _|  _|   "
               "     _|     "
               "   _|  _|   "
               " _|      _| "
               "            "
               "            ")
         (gethash #\X table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push  (list "            "
               " _|      _| "
               "   _|  _|   "
               "     _|     "
               "     _|     "
               "     _|     "
               "            "
               "            ")
         (gethash #\Y table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; otherwise {{{
  (push (list "    "
              "    "
              "    "
              "    "
              "    "
              " _| "
              "    "
              "    ")
        (gethash #\. table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "        "
              " _|_|   "
              "     _| "
              " _|_|   "
              "        "
              " _|     "
              "        "
              "        ")
        (gethash #\? table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "    "
              " _| "
              " _| "
              " _| "
              "    "
              " _| "
              "    "
              "    ")
        (gethash #\! table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (push (list "            "
              "            "
              "            "
              " _|_|_|_|_| "
              "            "
              "            "
              "            "
              "            ")
        (gethash #\- table))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (defun char-list->aa-list (char-list &key (background nil))
    (let ((result nil))
      (dotimes (i 8)
        (push (apply #'mkstr (mapcar (lambda (x)
                                       (nth i (car (gethash x table))))
                                     char-list)) result)
        (if background 
          (push (replstr " " background (pop result)) result)))
      (nreverse result))))

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

