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
that embedded single-quote characters by escaped by doubling them.

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
  (apply #'sb-ext:run-program
         *gnuplot-program*
         (mapcar #'stringify-arg (ensure-list options))
         :search search :wait wait :input input :output output :error error
         args))

(defmacro with-output-to-gnuplot ((symbol process) &body body)
  `(with-open-stream (,symbol (sb-ext:process-input ,process)) ,@body))

@ Here's the primary interface function. It takes a list of command-line
options followed by any number of command designators. Lists whose first
element is |string=| to |:plot| or |:splot| are treated specially; we'll
get to the specifics in a moment. All other command deisgnators are
mechanically translated to gnuplot syntax via pretty-printing.

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
|:ranges|, for which we offer a bit of extra support---|(:ranges x y)| gets
translated to \.{"[x] [y]"} and inserted near the beginning of the
command---each subform, or {\it clause}, is treated as a specification for
a plot. When all of the clauses have been analyzed, they are spliced
together with commas to form the complete command.

We do not attempt a detailed analysis of the clauses, as that would require
far too much knowledge of the (fairly intricate) syntax of the plot command.
What we do offer, though, is support for {\it inline data sources}. The idea
here is that you have (say) a Lisp vector full of data which you'd like to
plot; rather than writing it out to a file and having gnuplot read it back
in, we can replace the data source in the command specification with the
special filename \.{'-'}, which tells gnuplot to read the data from its
standard input. We then return all of the inline data sources along with
the reconstructed command.

@l
(defun parse-plot-command (command)
  (assert (string= (car command) :plot)
          (command)
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

@ The only type of inline data source we currently support is non-string
arrays. We push them onto the |sources| list and substitute the pathname
\.{'-'}.

@<Replace inline...@>=
(loop for x in clause
      collect (typecase x
                ((and array (not string)) (push x sources) #P"-")
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
    (vector (map nil (lambda (x) (format t "~F~%" x)) data))
    ((array * *)
     (dotimes (i (array-dimension data 0))
       (dotimes (j (array-dimension data 1) (terpri))
         (format t "~F " (aref data i j))))))
  (write-line "e"))

@t A simple test of the whole shebang.

@l
(deftest plot
  (sb-ext:process-p
   (gnuplot :persist
     '(:set :key :off)
     '(:plot (:ranges "0:2*pi")
             (#(1 2 3) :with :lines)
             (#(4 5 6) :with :lines)
             "sin(x)")))
  t)

@ Here's a little convenience function for plotting a single source
with some common options.

@l
(defun ensure-key (key)
  (case key ((t) :on) ((nil) :off) (t key)))

(defun maybe-ranges (ranges)
  (if ranges `((:ranges ,@(ensure-list ranges)))))

(defun plot (source &key (options '(:persist)) key (term "x11") ranges ;
             (with :lines))
  (gnuplot options
    `(:set :term ,term)
    `(:set :key ,(ensure-key key))
    `(plot ,@(maybe-ranges ranges) (,source :with ,with))))

@ And here's a related one that relies on a bit of Emacs hackery to perform
its magic; see \.{inline-images.el}.

@l
(defun plot-inline (source &key options key (term "png") ranges ;
                    (with :lines) (output-file #P"/tmp/plot.png"))
  (sb-ext:process-wait
   (gnuplot options
     `(:set :term ,term)
     `(:set :output ,output-file)
     `(:set :key ,(ensure-key key))
     `(:plot ,@(maybe-ranges ranges) (,source :with ,with))))
  (probe-file output-file))
