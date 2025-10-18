(asdf:defsystem "hiccl"
  :description "HTML generator for Common Lisp"
  :version "2.0"
  :author "garlic0x1"
  :license "MIT"
  :components ((:file "hiccl"))
  :in-order-to ((test-op (test-op "hiccl/test"))))

(asdf:defsystem "hiccl/test"
  :depends-on ("hiccl" "fiasco" "alexandria" "plump" "lquery")
  :components ((:file "hiccl-tests"))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:fiasco '#:run-tests '#:hiccl/test)))
