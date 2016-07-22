
(load "../lib/stdlib")
(load "to-html")
(load "node")

(defun parse-xml (str)
  (labels ((rec (str acc)
                (cond ((not (position #\< str)))
                (let ((has-node? (position #\< str)))
                  (cond ((has-node?
                           (unless (/= has-node? 0)
                             (push (before #\< str) acc))
                           (let* ((node-name (do ((i (1+ has-node?) (1+ i)))
                                               ((char= (char str i) #\>) (subseq str (1+ has-node?) i))))
                                  (node-end (do ((j (position #\> str) (1+ j))
                                                 (buf "" (mkstr buf (char str j))))
                                              ((string-equal buf (mkstr "</" node-name ">")) j )
                                              (when (and (not (empty? buf))
                                                         (or (char/= (char buf 0) #\<)
                                                             (char= (char buf (1- (length buf))) #\>)))
                                                (setq buf "")))))
                             (push (subseq str has-node? node-end) acc)
                             (if (>= node-end (length str))
                               (nreverse acc)
                               (rec (subseq str (1+ node-end)) acc)))))
                        (t
                          (push str acc)
                          (nreverse acc))))))
    (make-node :child-nodes (rec str nil))))


(trace parse-xml)
(print-nodes (parse-xml "<html> hogehoge </html>"))

