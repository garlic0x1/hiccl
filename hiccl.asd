(asdf:defsystem "hiccl"
  :description "HTML generator for Common Lisp"
  :version "1.0"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:serapeum :trivia :str)
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "expand")
                             (:file "sanitize")
                             (:file "render")))))
