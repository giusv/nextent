(in-package :gui)

(defprim label (expr)
  (:pretty () (list 'label (list :expr (synth :pretty expr))))
  (:req (*) (html:taglist 
             (doc:text "Etichetta contenente la seguente espressione: ") 
             (synth :req expr)))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil)
  (:template () (html:label (synth :template expr)))
  (:controller () (ng-empty))
  (:components (*) nil)
  (:routes (*) nil))
