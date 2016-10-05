
(require :stdlib *module-stdlib*)

(let ((index 0)
      (key-arr)
      (key-length))
  (defun get-key-value (key)
    (when (null key-length)
      (setq key-length (length key)
            key-arr (string->byte key)))
    (aref key-arr (mod index key-length))
    (incf index)))

(defun crypt-main (key file)
  (let ((buf)
        (element-type '(unsigned-byte 8)))
    (with-open-file (in file :direction :input :element-type element-type)
      (setq buf (make-array (file-length in) :element-type element-type))
      (read-sequence buf in))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :element-type element-type)
      (dotimes (i (length buf)) 
        (write-byte
          (boole boole-xor (aref buf i) (get-key-value key))
          out)))))

(defexe crypt (--help)
  "help"
  (let ((usage (usage :title "crypt [OPTION] KEY FILE"
                      :desc  "Encrypt(Decrypt) the file."))
        (key (first args))
        (file (second args)))
    (cond
      ((or --help
           errors
           (null key)
           (null file))
       (funcall usage))
      ((not (file-exists? file))
       (format *error-output* "crypt: no such file or directory `~A'~%" file)
       (funcall usage))
      (t
        (crypt-main key file)))))

