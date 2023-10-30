(asdf:defsystem "hiccl-test"
  :author "garlic0x1"
  :license "MIT"
  :depends-on ("fiveam"
               "plump"
               "lquery"
               "hiccl")
  :components ((:module "t"
                :components
                ((:file "hiccl-test"))))
  )
