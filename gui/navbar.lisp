(defprim navbar (name &rest links)
  (:pretty () (list 'navbar (list :name name :links (synth-all :pretty links))))
  (:html (path) (multitags (text "Barra di navigazione ")
                             (if (not  links) 
                                 (text "vuota") 
                                 (text "composta dai seguenti link:"))
                             (apply #'ul nil ;; (list :class 'list-group)
                                    (mapcar #'listify (synth-all :html links path)))))
  (:brief (path) (synth :html this path))
  
  (:reqlist (*) nil)
  (:template () (html:nav 
                    :|class| "navbar navbar-default"
                    (synth-all :template links)))

  (:controller () (ng-empty))
  (:components (*) nil)
  (:routes (father) nil))

