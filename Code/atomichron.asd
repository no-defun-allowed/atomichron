(asdf:defsystem :atomichron
  :depends-on (:bordeaux-threads :atomics)
  :components ((:file "package")
               (:file "meter" :depends-on ("package"))))
