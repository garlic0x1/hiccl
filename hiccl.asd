(defsystem "hiccl"
  :description "Hiccup for Common Lisp"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:serapeum)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "sanitize")
                 (:file "render")))))
