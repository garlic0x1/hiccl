(asdf:defsystem "hiccl"
  :description "Hiccup for Common Lisp"
  :version "0.1"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:serapeum
               :binding-arrows
               :trivia
               :str)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "expand")
                 (:file "validate")
                 (:file "sanitize")
                 (:file "render")))))
