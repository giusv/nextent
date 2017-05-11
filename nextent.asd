;;;; nextent.asd

(asdf:defsystem :nextent
  :serial t
  :description "Describe nextent here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "lol")
               (:file "utils")
               (:file "parser")
               (:file "grammar")
               (:file "doc")
               (:file "html")
               (:file "expression")
               (:file "url")
               (:module "lang"
                        :serial t
                        :components ((:file "blub")))
               (:module "data"
                        :serial t
                        :components ((:file "data")
                                     (:file "json")
                                     (:file "jsonschema")))
               (:module "server"
                        :serial t
                        :components ((:file "rest")))
               (:module "gui"
                        :serial t
                        :components ((:file "input")
                                     (:file "button")
                                     (:file "link")
                                     (:file "navbar")
                                     (:file "vert")
                                     (:file "horz")
                                     (:file "label")
                                     (:file "panel")
                                     (:file "listing")
                                     (:file "table")
                                     (:file "description")
                                     (:file "abst")
                                     (:file "static")
                                     (:file "dynamic")
                                     (:file "alt")
                                     (:file "form"))) 
               (:file "nextent")))


