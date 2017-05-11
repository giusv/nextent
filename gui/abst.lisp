(in-package :gui)

(defprim abst (parameters element)
  (:pretty () (list 'abst (list :parameters (synth-all :pretty parameters) 
                                :element (synth :pretty element)))) 
  (:req (path) (html:taglist (html:h5 (doc:text "Input")) 
                             (doc:text "Elemento parametrico caratterizzato dai seguenti parametri:")
                             (html:table :style "width: auto;" 
                                         :class "table table-striped"
                                         (html:tr 
                                          (html:th (doc:text "Parametro"))
                                          (html:th (doc:text "Tipo")))
                                         (mapcar #'(lambda (par)
                                                     (html:tr 
                                                      (html:td (doc:text "~a" (synth :name par)))
                                                      (html:td (synth :type par))))
                                                 parameters))
                             (synth :req element path)))
  (:brief (path) (synth :req this path))
  (:reqlist (path) (synth :reqlist element path))
  (:template () (synth :template element))
  (:controller () (bb-empty))
  (:components (father) (synth :components element father))
  (:routes (father) (synth :routes element father))
  (:imports () (synth :imports element))
  (:dependencies () (synth :dependencies element)))
