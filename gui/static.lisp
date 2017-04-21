(in-package :gui)

(defprim static% (name element)
  (:pretty () (list 'static (list :name name :element (synth :pretty element))))
  (:brief (path) (let ((newpath (backward-chain (static-chunk name) path)))
                   (html:strong 
                    (doc:text "~a (URL: " (upper-camel name #\Space)) 
                    (html:a :href (doc:text "#~a" (synth :string (synth :url newpath) 0))
                            (html:code (synth :url newpath)))
                    (doc:text ")"))))

  (:req (path) (let ((newpath (backward-chain (static-chunk name) path)))
                 (html:taglist 
                  (html:section 
                   (html:h4 nil (doc:text "~a (URL: " (upper-camel name #\Space)) 
                            (html:code :id (synth :string (synth :url newpath) 0) 
                                       (synth :url newpath))
                            (doc:text ")"))
                   (synth :req element newpath)))))
  (:reqlist (path) 
            (let ((newpath (backward-chain (static-chunk name) path))) 
              (cons (synth :req this path)
                    (synth :reqlist element newpath))))
  (:template (&optional father) (html:tag name))
  (:controller () nil) 
  (:components () (list (ng-unit name
                                 (ng-import (ng-const "@angular/core") 'component)
                                 (ng-primitive 'component
                                               :selector (ng-const (string-downcase name))
                                               :template (ng-template (synth :template element)))
                                 (ng-class (symb name "-COMPONENT"))))))

(defmacro static (name queries element)
  `(static% ,name 
            ,(if queries 
                 `(let ,(mapcar #'(lambda (query) 
                                    `(,query (query-parameter ',query)))
                                queries)
                    (abst (list ,@queries) ,element))
                 element)))
