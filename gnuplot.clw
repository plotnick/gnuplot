@*Gnuplot. This is a simple Lisp wrapper for gnuplot.

@ We begin with a package definition. We use a few SBCL-only packages:
|sb-ext| gives us process control, and |sb-rt| is Richard Waters's
regression testing library. Porting to other Common Lisp implementations
should be straightforward, but has not been attempted.

@l
@e
(defpackage "GNUPLOT"
  (:documentation "A simple Lisp interface to gnuplot.")
  (:use "COMMON-LISP" "SB-EXT" "SB-RT")
  (:export "*GNUPLOT*"
           "RUN-GNUPLOT" "WITH-OUTPUT-TO-GNUPLOT" "GNUPLOT"
           "PLOT" "WRITE-PLOT" "PLOT-INLINE" "PLOT-HISTOGRAM"))
@e
(in-package "GNUPLOT")
@e
(provide 'gnuplot)

@ This little utility function is primarily used to resolve list designators.

@l
(defun ensure-list (x)
  (if (listp x) x (list x)))

@ Gnuplot commands will be designated by Lisp objects, which we'll convert
to strings using the Lisp pretty printer with a specialized dispatch table.
The goal is to offer a transparent, Lispy interface instead of raw (string)
commands, but not to go much further than that.

@l
(defparameter *gnuplot-pprint-dispatch* (copy-pprint-dispatch nil))

(defun set-gnuplot-dispatch ;
    (type-specifier function &optional (priority 0)
     (*print-pprint-dispatch* *gnuplot-pprint-dispatch*))
  (set-pprint-dispatch type-specifier function priority))

(defmacro with-gnuplot-syntax (&body body)
  `(let ((*print-pretty* t)
         (*print-escape* nil)
         (*print-readably* nil)
         (*print-pprint-dispatch* *gnuplot-pprint-dispatch*)
         (*print-right-margin* 1000))
     ,@body))

@ Strings designate themselves in commands.

@l
(set-gnuplot-dispatch 'string
  (lambda (stream object)
    (write-string object stream)))

@ Symbols designate their name in lowercase.

@l
(set-gnuplot-dispatch 'symbol
  (lambda (stream object)
    (write-string (string-downcase (symbol-name object)) stream)))

@ We'll treat most lists as standing for commands with whitespace-separated
constituents. E.g., using the symbol dispatching we just defined, in a
|gnuplot| block you can say |(set key off)| instead of the equivalent
|"set key off"|.

@l
(set-gnuplot-dispatch 'list
  (lambda (stream object)
    (format stream "~{~W~^ ~}" object)))

@ We'll write pathnames using gnuplot's single-quote syntax, which requires
that embedded single-quote characters be escaped by doubling them.

@l
(defun escape (string chars &optional (escape #\\)
               &aux (chars (ensure-list chars)))
  (coerce (loop for ch across string
                when (member ch chars :test #'char=) collect escape
                collect ch)
          'string))

(set-gnuplot-dispatch 'pathname
  (lambda (stream object)
    (write-char #\' stream)
    (write-string (escape (namestring object) #\' #\') stream)
    (write-char #\' stream)))

@ We'll execute gnuplot using SBCL's |run-program|. We treat symbols
as designators for the same-named command-line options; keyword symbols
designate GNU-style long options (e.g., \.{--persist}), and all other
symbols designate X11-style options (e.g., \.{-clear}).

@l
(defvar *gnuplot-program* "gnuplot"
  "The name of the program to execute.")

(defun stringify-arg (x)
  (typecase x
    (symbol (format nil "~:[-~;--~]~(~A~)" (keywordp x) (symbol-name x)))
    (t (princ-to-string x))))

(defun run-gnuplot (options &rest args &key ;
                    (search t) (wait nil) (input :stream) (output t) (error t))
  (apply #'run-program
         *gnuplot-program*
         (mapcar #'stringify-arg (ensure-list options))
         :search search :wait wait :input input :output output :error error
         args))

(defmacro with-output-to-gnuplot ((symbol process) &body body)
  `(with-open-stream (,symbol (process-input ,process)) ,@body))

@ Here's the primary interface function. It takes a list of command-line
options followed by any number of command designators. Lists whose first
element is |string=| to |:plot| or |:splot| are treated specially; we'll
get to the specifics in a moment. All other command deisgnators are
mechanically translated to gnuplot syntax via pretty-printing.

Even though this is a function, I usually indent it like it's a macro whose
|cddr| is a body. You can tell GNU~Emacs to do this automatically like so:
|(put 'gnuplot 'lisp-indent-function 1)|.

@l
(defun gnuplot (options &rest commands)
  (flet ((plot-command-p (command)
           (and (listp command)
                (member (car command) '(:plot :splot) :test #'string=))))
    (let ((process (run-gnuplot options)))
      (with-output-to-gnuplot (*standard-output* process)
        (with-gnuplot-syntax
          (dolist (command commands process)
            (let (data)
              (when (plot-command-p command)
                (multiple-value-setq (command data) ;
                  (parse-plot-command command)))
              (write command)
              (terpri)
              (mapc #'write-inline-data data))))))))

@ Gnuplot's two primary plotting commands commands, \.{plot} and~\.{splot},
get special treatment. With the exception of a form beginning with the key
|:ranges|, for which we offer a bit of extra support---|(:ranges x y)|
is translated to \.{"[x] [y]"} and inserted near the beginning of the
command---each subform, or {\it clause}, is treated as a specification
for a plot. When all of the clauses have been analyzed, they are spliced
together with commas to form the complete command.

We do not attempt a detailed analysis of the clauses, as that would require
far too much knowledge of the (fairly intricate) syntax of the plot command.
What we do offer, though, is support for {\it inline data sources}. The idea
here is that you have (say) a Lisp sequence full of data which you'd like
to plot; rather than writing it out to a file and having gnuplot read it
back in, we replace the data source in the command specification with
the special filename \.{'-'}, which tells gnuplot to read the data from
standard input.

@l
(deftype data-source ()
  '(or (and array (not string)) (cons number)))

(deftype data-source-with-options ()
  '(cons data-source (cons keyword)))

(defun parse-plot-command (command)
  (assert (string= (car command) :plot) ;
          (command) ;
          "Invalid plot command ~S." command)
  (let (ranges plots sources)
    (with-gnuplot-syntax
      (dolist (clause (mapcar #'ensure-list (cdr command))
               (values (format nil "plot~@[ ~A~]~{ ~A~^,~}" ;
                               ranges (nreverse plots))
                       (nreverse sources)))
        (cond ((and (symbolp (car clause))
                    (string= (car clause) :ranges))
               (setq ranges (format nil "~{[~A]~^ ~}" (cdr clause))))
              (t (push @<Replace inline data sources in |clause|@> plots)))))))

@t@l
(defmacro define-parse-plot-test (name plot-form expected-command ;
                                  &optional data)
  `(deftest (parse-plot ,name)
     (parse-plot-command ,plot-form)
     ,expected-command
     ,data))

(define-parse-plot-test simple '(:plot "sin(x)") "plot sin(x)")

(define-parse-plot-test ranges
  '(:plot (:ranges "0:2*pi" "0:1") "sin(x)")
  "plot [0:2*pi] [0:1] sin(x)")

(define-parse-plot-test multi
  '(:plot ("sin(x)" :with :points)
          ("cos(x)" :with :lines)
          #P"foo's data")
  "plot sin(x) with points, cos(x) with lines, 'foo''s data'")

@ @<Replace inline data sources...@>=
(loop for x in clause
      collect (typecase x
                (data-source-with-options
                 (push (car x) sources)
                 `(,#P"-" ,@(cdr x)))
                (data-source
                 (push x sources)
                 #P"-")
                (t x)))

@t@l
(define-parse-plot-test inline-array
  '(:plot #(1 2 3 4 5))
  "plot '-'"
  (#(1 2 3 4 5)))

@ Writing the inline data sources is straightforward. We send newline-separated
records of space-separated ASCII representations of floating-point numbers,
followed by a special `end-of-data' marker line.

@l
(defun write-inline-data (data)
  (etypecase data
    (sequence (map nil (lambda (x) (format t "~F~%" x)) data))
    ((array * *)
     (dotimes (i (array-dimension data 0))
       (dotimes (j (array-dimension data 1) (terpri))
         (format t "~F " (aref data i j))))))
  (write-line "e"))

@t A simple test of the whole shebang.

@l
(deftest plot
  (process-alive-p
   (process-wait
    (gnuplot '(#+(or) :persist)
      '(:set :key :off)
      '(:plot (:ranges "0:2*pi")
        (#(1 2 3) :with :lines)
        (#(4 5 6) :with :lines)
        "sin(x)"))))
  nil)

@ Here's a little convenience function for plotting a (designator for a)
list of sources with some common options.

@l
(defun ensure-key (key)
  (case key
    ((t) :on)
    ((nil) :off)
    (t key)))

(defun maybe-ranges (ranges)
  (when ranges `((:ranges ,@(ensure-list ranges)))))

(defun plot (sources &key (options '(:persist)) (terminal "x11") (with :lines)
             ranges output-file title
             (key nil key-supplied-p) &allow-other-keys &aux
             (sources (etypecase sources
                        ((or data-source data-source-with-options)
                         (list sources))
                        (list sources))))
  (gnuplot options
    `(:set :terminal ,terminal)
    `(:set :output ,@(and output-file `(,output-file)))
    `(:set :key ,(ensure-key (if key-supplied-p
                                 key
                                 (and (some (lambda (source)
                                              (typecase source
                                                (data-source-with-options ;
                                                 (getf (cdr source) :title))))
                                            sources)
                                      t))))
    `(:set :title ,@(and title `(,title)))
    `(plot ,@(maybe-ranges ranges)
           ,@(loop for source in sources
                   collect `(,source :with ,with)))))

@ And here's a related one that relies on a bit of Emacs hackery to perform
its magic; see \.{inline-images.el}.

@l
(defun write-plot (sources &rest args &key (options '())
                   (plot #'plot)
                   (terminal "eps")
                   (output-file (make-pathname :name "plot" :type terminal))
                   (wait t) &allow-other-keys)
  (let ((process (apply plot sources
                        :options options
                        :terminal terminal
                        :output-file output-file
                        args)))
    (cond (wait (process-wait process) (probe-file output-file))
          (t process))))

(defun plot-inline (sources &rest args &key (terminal "png")
                    (output-file #P"/tmp/plot.png") &allow-other-keys)
  (apply #'write-plot sources
         :terminal terminal
         :output-file output-file
         args))

@ Gnuplot can draw histograms, but is clumsy at binning---we're better
off doing it in Lisp. We support only constant-width bins for now.

@l
(defun histogram (data &key (bins (ceiling (sqrt (length data))))
                  (min (reduce #'min data))
                  (max (reduce #'max data)) &aux
                  (width (/ (- max min) (1- bins)))
                  (histogram @<Make a histogram array@>))
  @<Fill the first histogram column@>
  @<Fill the second histogram column@>
  histogram)

(defun plot-histogram (data &rest args &key (options '(:persist)) key ranges ;
                       title (terminal "x11") output-file
                       (border 0) (with :boxes) &allow-other-keys)
  (let ((histogram (apply #'histogram data :allow-other-keys t args)))
    (gnuplot options
      `(:set :terminal ,terminal)
      `(:set :output ,@(and output-file `(,output-file)))
      `(:set :style :histogram)
      `(:set :style :fill :solid 0.5 :border :lt -1)
      `(:set :key ,(ensure-key key))
      `(:set :title ,@(and title `(,title)))
      `(:set :border ,border)
      `(:set :xtics :nomirror)
      `(:unset :ytics)
      `(plot ,@(maybe-ranges ranges) (,histogram :with ,with)))))

@ We represent histograms as two-dimensional arrays, with a row for each bin.

@<Make a histogram array@>=
(make-array (list bins 2) :initial-element 0)

@ The first column contains the coordinates of the left-hand bin edges.

@<Fill the first histogram column@>=
(loop for bin below bins
      as edge = min then (+ edge width)
      do (setf (aref histogram bin 0) edge))

@ And the second column contains the bin counts.

@<Fill the second histogram column@>=
(map nil (lambda (x) (incf (aref histogram (floor (- x min) width) 1))) data)

@*Index.
@t*Index.
