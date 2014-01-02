(asdf:defsystem #:naclcl
  :serial t
  :description "A port of the NaCl library for cryptalgorithms."
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "Public domain"
  :components ((:file "package")
               (:file "naclcl")))
