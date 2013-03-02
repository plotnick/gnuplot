@ @l
(defun make-plot-command (specs ranges &aux (specs (ensure-list specs)))
  (let ((plots '())
        (data '()))
    (dolist (spec specs)
      (destructuring-bind (source &key using with) (ensure-list spec)
        (push (format nil " ~A~@[ using ~A~]~@[ with ~A~]"
                      (etypecase source
                        (string source)
                        (pathname (format nil "'~A'" (namestring source)))
                        (array (push source data) "'-'"))
                      using
                      (etypecase with
                        (null nil)
                        (string with)
                        (symbol (string-downcase (symbol-name with)))))
              plots)))
    (values (format nil "plot~@[ ~A~]~{~A~^,~}~%"
                    (typecase ranges
                      (cons (format nil "~{[~A]~^ ~}" ranges))
                      (t ranges))
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

(defun plot (specs &key key ranges (output t) (error t) (pause t) (term "x11"))
  (let ((process (sb-ext:run-program "gnuplot" ()
                                     :search t
                                     :wait nil
                                     :input :stream
                                     :output output
                                     :error error)))
    (with-open-stream (*standard-output* (sb-ext:process-input process))
      (format t "set term ~A~%" term)
      (format t "set key ~A~%" (case key ((t) "on") ((nil) "off") (t key)))
      (multiple-value-bind (command data) (make-plot-command specs ranges)
        (write-string command)
        (mapc #'write-inline-data data))
      (when pause
        (write-line "pause mouse key")))
    process))

(defun plot-inline (specs &rest args)
  (with-open-file (out "/tmp/plot.png" :direction :output :if-exists :supersede)
    (apply 'plot specs :output out :pause nil :term "png" args)
    (pathname out)))

