(asdf:defsystem "hiccl"
  :description "Hiccup for Common Lisp"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:serapeum :trivia :str)
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "expand")
                             (:file "sanitize")
                             (:file "render")))))
