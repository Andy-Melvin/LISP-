(defparameter *markov-table* (make-hash-table))

(defun learn (words)
  (loop for (w1 w2 w3) on words by #'cdr do
    (let ((key (list w1 w2)))
      (push w3 (gethash key *markov-table*)))))


(defun generate-text (length)
  (let* ((start (random-elt (hash-table-keys *markov-table*)))
         (w1 (first start))
         (w2 (second start))
         (output (list w1 w2)))
    (dotimes (n (- length 2))
      (let* ((key (list w1 w2))
             (next (random-elt (gethash key *markov-table*))))
        (push next output)
        (setf w1 w2
              w2 next)))
    (format nil "~{~a ~}" (nreverse output))))

(defun random-elt (list)
  (nth (random (length list)) list))

(defun hash-table-keys (hash-table)
  (let ((keys '()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun words (string)
  (remove-if #'null (split-sequence:split-sequence #\Space string)))

(defun read-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun train (filename)
  (learn (words (read-file filename))))

(defun generate (filename length)
  (train filename)
  (generate-text length))
