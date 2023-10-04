(defsystem :tl
  :serial t
  :depends-on (:bencode
               :cl-ascii-table
               :cl-ppcre
               :external-program
               :jonathan
               :uuid)
  :components ((:file "package")
               (:file "tl")))
