@ @l
(defun ensure-list (x)
  (if (listp x) x (list x)))

@ @l
(defun escape (string chars &optional (escape #\\)
               &aux (chars (ensure-list chars)))
  (coerce (loop for ch across string
                when (member ch chars :test #'char=) collect escape
                collect ch)
          'string))

(defun stringify (x)
  (etypecase x
    (null nil)
    (string x)
    (pathname (format nil "'~A'" (escape (namestring x) #\' #\')))
    (symbol (string-downcase (symbol-name x)))))

(defun stringify-arg (x)
  (if (keywordp x)
      (format nil "--~(~A~)" (symbol-name x))
      (stringify x)))

@ @l
(defun make-plot-command (specs ranges &aux (specs (ensure-list specs)))
  (let ((plots '())
        (data '()))
    (dolist (spec specs)
      (destructuring-bind (source &key using with) (ensure-list spec)
        (push (format nil " ~A~@[ using ~A~]~@[ with ~A~]"
                      (typecase source
                        (string source)
                        (array (push source data) "'-'")
                        (t (stringify source)))
                      (stringify using)
                      (stringify with))
              plots)))
    (values (format nil "plot~@[ ~A~]~{~A~^,~}~%"
                    (typecase ranges
                      (cons (format nil "~{[~A]~^ ~}" ranges))
                      (t (stringify ranges)))
                    (nreverse plots))
            (nreverse data))))

(defun write-inline-data (data)
  (etypecase data
    (vector (map nil (lambda (x) (format t "~F~%" x)) data))
    ((array * *)
     (dotimes (i (array-dimension data 0))
       (dotimes (j (array-dimension data 1) (terpri))
         (format t "~F " (aref data i j))))))
  (write-line "e"))

(defun plot (specs &key (args '(:persist)) (output t) (error t)
             key ranges (term "x11"))
  (let ((process (sb-ext:run-program "gnuplot" (mapcar #'stringify-arg args)
                                     :search t
                                     :wait nil
                                     :input :stream
                                     :output output
                                     :error error)))
    (with-open-stream (*standard-output* (sb-ext:process-input process))
      (format t "set term ~A~%" (stringify term))
      (format t "set key ~A~%" (case key
                                 ((t) "on")
                                 ((nil) "off")
                                 (t (stringify key))))
      (multiple-value-bind (command data) (make-plot-command specs ranges)
        (write-string command)
        (mapc #'write-inline-data data)))
    process))

(defun plot-inline (specs &rest args)
  (with-open-file (out "/tmp/plot.png" :direction :output :if-exists :supersede)
    (apply 'plot specs :args '() :output out :term "png" args)
    (pathname out)))

