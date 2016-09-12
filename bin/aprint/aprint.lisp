
(require "stdlib" *module-stdlib*)
(require "regex" *module-regex*)

(defparameter *surround-character* nil)
(defparameter *bg-character* #\Space)

(defun make-print-table ()
  (let ((table (make-hash-table :size 80)))
    ;; number {{{
    ;; 0 {{{

    (setf (gethash #\0 table)
          (list "        "
                "   _|   "
                " _|  _| "
                " _|  _| "
                " _|  _| "
                "   _|   "
                "        "
                "        "))

    ;; }}}
    ;; 1 {{{

    (setf (gethash #\1 table)
          (list "        "
                "   _|   "
                " _|_|   "
                "   _|   "
                "   _|   "
                "   _|   "
                "        "
                "        "))

    ;; }}}
    ;; 2 {{{

    (setf (gethash #\2 table)
          (list "          "
                "   _|_|   "
                " _|    _| "
                "     _|   "
                "   _|     "
                " _|_|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; 3 {{{

    (setf (gethash #\3 table)
          (list "          "
                " _|_|_|   "
                "       _| "
                "   _|_|   "
                "       _| "
                " _|_|_|   "
                "          "
                "          "))

    ;; }}}
    ;; 4 {{{

    (setf (gethash #\4 table)
          (list "          "
                " _|  _|   "
                " _|  _|   "
                " _|_|_|_| "
                "     _|   "
                "     _|   "
                "          "
                "          "))

    ;; }}}
    ;; 5 {{{

    (setf (gethash #\5 table)
          (list "          "
                " _|_|_|_| "
                " _|       "
                " _|_|_|   "
                "       _| "
                " _|_|_|   "
                "          "
                "          "))

    ;; }}}
    ;; 6 {{{

    (setf (gethash #\6 table)
          (list "          "
                "   _|_|_| "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                "   _|_|   "
                "          "
                "          "))

    ;; }}}
    ;; 7 {{{

    (setf (gethash #\7 table)
          (list "          "
                " _|_|_|_| "
                "      _|  "
                "    _|    "
                "   _|     "
                "  _|      "
                "          "
                "          "))

    ;; }}}
    ;; 8 {{{

    (setf (gethash #\8 table)
          (list "          "
                "   _|_|   "
                " _|    _| "
                "   _|_|   "
                " _|    _| "
                "   _|_|   "
                "          "
                "          "))

    ;; }}}
    ;; 9 {{{

    (setf (gethash #\9 table)
          (list "          "
                "   _|_|   "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                " _|_|_|   "
                "          "
                "          "))

    ;; }}}
    ;; }}}
    ;; [a-z] {{{
    ;; a {{{

    (setf (gethash #\a table)
          (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; b {{{

    (setf (gethash #\b table)
          (list "          "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|_|_|   "
                "          "
                "          "))

    ;; }}}
    ;; c {{{

    (setf (gethash #\c table)
          (list "          "
                "          "
                "   _|_|_| "
                " _|       "
                " _|       "
                "   _|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; d {{{

    (setf (gethash #\d table)
          (list "       _| "
                "       _| "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; e {{{

    (setf (gethash #\e table)
          (list "          "
                "          "
                "   _|_|   "
                " _|_|_|_| "
                " _|       "
                "   _|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; f {{{

    (setf (gethash #\f table)
          (list "          "
                "     _|_| "
                "   _|     "
                " _|_|_|_| "
                "   _|     "
                "   _|     "
                "          "
                "          "))

    ;; }}}
    ;; g {{{

    (setf (gethash #\g table)
          (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "   _|_|   "))

    ;; }}}
    ;; h {{{

    (setf (gethash #\h table)
          (list "          "
                " _|       "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "          "
                "          "))

    ;; }}}
    ;; i {{{

    (setf (gethash #\i table)
          (list "     "
                "  _| "
                "     "
                "  _| "
                "  _| "
                "  _| "
                "     "
                "     "))

    ;; }}}
    ;; i {{{

    (setf (gethash #\j table)
          (list "      "
                "   _| "
                "      "
                "   _| "
                "   _| "
                "   _| "
                "  _|  "
                " _|   "))

    ;; }}}
    ;; k {{{

    (setf (gethash #\k table)
          (list "          "
                " _|       "
                " _|  _|   "
                " _|_|     "
                " _|  _|   "
                " _|    _| "
                "          "
                "          "))

    ;; }}}
    ;; l {{{

    (setf (gethash #\l table)
          (list "     "
                "  _| "
                "  _| "
                "  _| "
                "  _| "
                "  _| "
                "     "
                "     "))

    ;; }}}
    ;; m {{{

    (setf (gethash #\m table)
          (list "              "
                "              "
                " _|_|_| _|_|  "
                " _|   _|   _| "
                " _|   _|   _| "
                " _|   _|   _| "
                "              "
                "              "))

    ;; }}}
    ;; n {{{

    (setf (gethash #\n table)
          (list "          "
                "          "
                "  _|_|_|  "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "          "
                "          "))

    ;; }}}
    ;; o {{{

    (setf (gethash #\o table)
          (list "          "
                "          "
                "   _|_|   "
                " _|    _| "
                " _|    _| "
                "   _|_|   "
                "          "
                "          "))

    ;; }}}
    ;; p {{{

    (setf (gethash #\p table)
          (list "          "
                "          "
                " _|_|_|   "
                " _|    _| "
                " _|    _| "
                " _|_|_|   "
                " _|       "
                " _|       "))

    ;; }}}
    ;; p {{{

    (setf (gethash #\q table)
          (list "          "
                "          "
                "   _|_|_| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "       _| "))

    ;; }}}
    ;; r {{{

    (setf (gethash #\r table)
          (list "          "
                "          "
                " _|  _|_| "
                " _|_|     "
                " _|       "
                " _|       "
                "          "
                "          "))

    ;; }}}
    ;; s {{{

    (setf (gethash #\s table)
          (list "          "
                "          "
                "   _|_|_| "
                " _|_|     "
                "     _|_| "
                " _|_|_|   "
                "          "
                "          "))

    ;; }}}
    ;; t {{{

    (setf (gethash #\t table)
          (list "          "
                "   _|     "
                " _|_|_|_| "
                "   _|     "
                "   _|     "
                "     _|_| "
                "          "
                "          "))

    ;; }}}
    ;; u {{{

    (setf (gethash #\u table)
          (list "          "
                "          "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "          "
                "          "))

    ;; }}}
    ;; v {{{

    (setf (gethash #\v table)
          (list "               "
                "               "
                "  _|        _| "
                "   _|      _|  "
                "     _|  _|    "
                "       _|      "
                "               "
                "               "))

    ;; }}}
    ;; w {{{

    (setf (gethash #\w table)
          (list "                    "
                "                    "
                " _|      _|      _| "
                "  _|     _|     _|  "
                "   _|  _|  _|  _|   "
                "     _|      _|     "
                "                    "
                "                    "))

    ;; }}}
    ;; x {{{

    (setf (gethash #\x table)
          (list "           "
                "           "
                "  _|    _| "
                "    _|_|   "
                "    _|_|   "
                "  _|    _| "
                "           "
                "           "))

    ;; }}}
    ;; y {{{

    (setf (gethash #\y table)
          (list "          "
                "          "
                " _|    _| "
                " _|    _| "
                " _|    _| "
                "   _|_|_| "
                "       _| "
                "   _|_|   "))

    ;; }}}
    ;; z {{{

    (setf (gethash #\z table)
          (list "           "
                "           "
                "  _|_|_|_| "
                "      _|   "
                "    _|     "
                "  _|_|_|_| "
                "           "
                "           "))

    ;; }}}
    ;; }}}
    ;; [A-Z] {{{
    ;; A {{{

    (setf  (gethash #\A table)
           (list "          "
                 "   _|_|   "
                 " _|    _| "
                 " _|_|_|_| "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          "))

    ;; }}}
    ;; B {{{

    (setf  (gethash #\B table)
           (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; C {{{

    (setf  (gethash #\C table)
           (list "          "
                 "   _|_|_| "
                 " _|       "
                 " _|       "
                 " _|       "
                 "   _|_|_| "
                 "          "
                 "          "))

    ;; }}}
    ;; D {{{

    (setf  (gethash #\D table)
           (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 " _|_|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; E {{{

    (setf  (gethash #\E table)
           (list "          "
                 " _|_|_|_| "
                 " _|       "
                 " _|_|_|   "
                 " _|       "
                 " _|_|_|_| "
                 "          "
                 "          "))

    ;; }}}
    ;; F {{{

    (setf  (gethash #\F table)
           (list "          "
                 " _|_|_|_| "
                 " _|       "
                 " _|_|_|   "
                 " _|       "
                 " _|       "
                 "          "
                 "          "))

    ;; }}}
    ;; G {{{

    (setf  (gethash #\G table)
           (list "          "
                 "   _|_|_| "
                 " _|       "
                 " _|  _|_| "
                 " _|    _| "
                 "   _|_|_| "
                 "          "
                 "          "))

    ;; }}}
    ;; H {{{

    (setf  (gethash #\H table)
           (list "          "
                 " _|    _| "
                 " _|    _| "
                 " _|_|_|_| "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          "))

    ;; }}}
    ;; I {{{

    (setf  (gethash #\I table)
           (list "        "
                 " _|_|_| "
                 "   _|   "
                 "   _|   "
                 "   _|   "
                 " _|_|_| "
                 "        "
                 "        "))

    ;; }}}
    ;; J {{{

    (setf  (gethash #\J table)
           (list "          "
                 "       _| "
                 "       _| "
                 "       _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; K {{{

    (setf  (gethash #\K table)
           (list "          "
                 " _|    _| "
                 " _|  _|   "
                 " _|_|     "
                 " _|  _|   "
                 " _|    _| "
                 "          "
                 "          "))

    ;; }}}
    ;; L {{{

    (setf  (gethash #\L table)
           (list "          "
                 " _|       "
                 " _|       "
                 " _|       "
                 " _|       "
                 " _|_|_|_| "
                 "          "
                 "          "))

    ;; }}}
    ;; M {{{

    (setf  (gethash #\M table)
           (list "           "
                 "_|      _| "
                 "_|_|  _|_| "
                 "_|  _|  _| "
                 "_|      _| "
                 "_|      _| "
                 "           "
                 "           "))

    ;; }}}
    ;; N {{{

    (setf  (gethash #\N table)
           (list "             "
                 "  _|      _| "
                 "  _|_|    _| "
                 "  _|  _|  _| "
                 "  _|    _|_| "
                 "  _|      _| "
                 "             "
                 "             "))

    ;; }}}
    ;; O {{{

    (setf  (gethash #\O table)
           (list "          "
                 "   _|_|   "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; P {{{

    (setf  (gethash #\P table)
           (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|       "
                 " _|       "
                 "          "
                 "          "))

    ;; }}}
    ;; Q {{{

    (setf  (gethash #\Q table)
           (list "            "
                 "   _|_|     "
                 " _|    _|   "
                 " _|  _|_|   "
                 " _|    _|   "
                 "   _|_|  _| "
                 "            "
                 "            "))

    ;; }}}
    ;; R {{{

    (setf  (gethash #\R table)
           (list "          "
                 " _|_|_|   "
                 " _|    _| "
                 " _|_|_|   "
                 " _|    _| "
                 " _|    _| "
                 "          "
                 "          "))

    ;; }}}
    ;; S {{{

    (setf  (gethash #\S table)
           (list "          "
                 "   _|_|_| "
                 " _|       "
                 "   _|_|   "
                 "       _| "
                 " _|_|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; T {{{

    (setf  (gethash #\T table)
           (list "            "
                 " _|_|_|_|_| "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "            "
                 "            "))

    ;; }}}
    ;; U {{{

    (setf  (gethash #\U table)
           (list "          "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 " _|    _| "
                 "   _|_|   "
                 "          "
                 "          "))

    ;; }}}
    ;; V {{{

    (setf  (gethash #\V table)
           (list "               "
                 " _|         _| "
                 "  _|       _|  "
                 "   _|     _|   "
                 "     _|  _|    "
                 "       _|      "
                 "               "
                 "               "))

    ;; }}}
    ;; W {{{

    (setf  (gethash #\W table)
           (list "                "
                 " _|          _| "
                 " _|          _| "
                 " _|    _|    _| "
                 "   _|  _|  _|   "
                 "     _|  _|     "
                 "                "
                 "                "))

    ;; }}}
    ;; X {{{

    (setf  (gethash #\X table)
           (list "            "
                 " _|      _| "
                 "   _|  _|   "
                 "     _|     "
                 "   _|  _|   "
                 " _|      _| "
                 "            "
                 "            "))

    ;; }}}
    ;; Y {{{

    (setf  (gethash #\Y table)
           (list "            "
                 " _|      _| "
                 "   _|  _|   "
                 "     _|     "
                 "     _|     "
                 "     _|     "
                 "            "
                 "            "))

    ;; }}}
    ;; Z {{{

    (setf  (gethash #\Z table)
           (list "            "
                 " _|_|_|_|_| "
                 "       _|   "
                 "     _|     "
                 "   _|       "
                 " _|_|_|_|_| "
                 "            "
                 "            "))

    ;; }}}
    ;; }}}
    ;; etc {{{
    ;; . {{{

    (setf (gethash #\. table)
          (list "    "
                "    "
                "    "
                "    "
                "    "
                " _| "
                "    "
                "    "))

    ;; }}}
    ;; ? {{{

    (setf (gethash #\? table)
          (list "        "
                " _|_|   "
                "     _| "
                " _|_|   "
                "        "
                " _|     "
                "        "
                "        "))

    ;; }}}
    ;; ! {{{

    (setf (gethash #\! table)
          (list "    "
                " _| "
                " _| "
                " _| "
                "    "
                " _| "
                "    "
                "    "))

    ;; }}}
    ;; - {{{

    (setf (gethash #\- table)
          (list "            "
                "            "
                "            "
                " _|_|_|_|_| "
                "            "
                "            "
                "            "
                "            "))

    ;; }}}
    ;; #\Space  {{{

    (setf (gethash #\Space table)
          (list "    "
                "    "
                "    "
                "    "
                "    "
                "    "
                "    "
                "    "))

    ;; }}}
    ;; }}}
    table))

(defparameter *print-table* (make-print-table))

(defun char-list->print-lines (char-list &key (background nil))
  (let ((result)
        (line))
    (dotimes (i 8)
      (setq line (apply #'mkstr
                        (mapcar (lambda (x)
                                  (aif (nth i (gethash x *print-table*))
                                    it
                                    (error "char-list->print-lines: Unknown character `~A' to print" x)))
                                char-list)))
      (push (if background
              (funcall #~s/ /${background}/g line)
              line)
            result))
    (nreverse result)))

(defun aprint (stream)
  (with-output-to-string (out)
    (awhile (read-line stream nil nil)
      (let* ((print-lines (char-list->print-lines
                            (coerce it 'list)
                            :background *bg-character*))
             (surround-line (if *surround-character*
                              (make-string (length (first print-lines))
                                           :initial-element *surround-character*)
                              +empty-string+)))
        (surround (princln surround-line out)
          (dolist (i print-lines)
            (princln  i out)))))))

