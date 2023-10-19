(defsystem "hiccl"
  :description "Hiccup for Common Lisp"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria
               :serapeum
               :str)
  :components ((:file "package")
               (:file "sanitize")
               (:file "render")))
