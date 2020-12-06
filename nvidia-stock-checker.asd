(asdf:defsystem #:nvidia-stock-checker
  :description "checks availability of nvidia's gpus"
  :build-operation "program-op"
  :build-pathname "nvidia-stock-checker"
  :entry-point "nvidia-stock-checker:main"
  :serial t
  :depends-on (#:dexador
               #:jsown
               #:cl-ascii-table)
  :components ((:file "package")
               (:file "nvidia-stock-checker")))
