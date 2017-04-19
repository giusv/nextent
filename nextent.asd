;;;; nextent.asd

(asdf:defsystem :nextent
  :serial t
  :description "Describe nextent here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "lol")
               (:file "parser")
               (:file "grammar")
               (:file "doc")
               (:file "html")
               ;; (:file "input")
               (:module "gui"
                        :serial t
                        :components ((:file "input")
                                     (:file "button")))
               (:module "web"
                        :serial t
                        :components ((:file "angular")))
               (:file "nextent")))


