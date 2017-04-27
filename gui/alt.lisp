(in-package :gui)
(defprim alt (default &rest elements)
  (:pretty () (list 'alt (:default (synth :pretty default) :elements (synth-all :pretty elements))))
  (:req (path)
        (html:taglist 
         (doc:text "Tale elemento mostra, a seconda dell'URL, una alternativa della lista")
         (if (not elements) (doc:text " vuota") (doc:text " seguente:"))
         (ul (if default (listify (synth :req default path))) 
             (mapcar #'listify (synth-all :brief elements path)))))
  (:brief (path) (synth :req this path))
  (:reqlist (path) (apply #'append (synth-all :req (cons default elements) path)))
  (:template () (html:tag 'router-outlet
                          (doc:empty))) 
  (:controller () (ng-empty))
  (:components (father) (apply #'append
                               (let ((unit-name (if father (symb father "-DEFAULT")
                                                    (symb "DEFAULT"))))
                                 (list (ng-unit unit-name
                                                (ng-import (ng-const "@angular/core") 'component)
                                                (ng-primitive 'component
                                                              :selector (ng-const (string-downcase unit-name))
                                                              :template (ng-template (synth :template default)))
                                                (ng-class (symb unit-name "-COMPONENT")
                                                          :fields (list (synth :controller default))))))
                               (synth-all :components elements father)))
  (:routes (father) (cons (ng-object :path (ng-const nil)
                                     :component (ng-static (mkstr father "-default-component")))
                          (apply #'append (synth-all :routes elements father)))))
