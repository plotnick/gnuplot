(require :clweb)

(defsystem "gnuplot"
  :description "A simple interface to gnuplot."
  :author "Alex Plotnick <shrike@netaxs.com>"
  :license "MIT"
  :depends-on ("sb-rt")
  :perform (test-op (op component) (symbol-call :sb-rt :do-tests))
  :components ((clweb:clweb-file "gnuplot")))
