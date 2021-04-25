(asdf:defsystem :atomichron
  :depends-on (:bordeaux-threads :atomics)
  :serial t
  :components ((:file "package")
               (:file "increment")
               (:file "sharding")
               (:file "meter")))
