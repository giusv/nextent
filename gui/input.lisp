(in-package :gui)

(defprim input (name label &key init)
  (:pretty () (list 'input (list :name name 
                                 :label (synth :pretty label)
                                 :init (synth :pretty init))))
  (:req (*) (html:taglist (doc:text "Campo di input identificato come")
                          (html:span-color (string-downcase name))
                          (doc:text " etichettato con ")
                          (synth :req label) 
                          ;; (if init
                          ;;     (dlist init (span nil (text "Valore iniziale")) (synth :req init)))
                          ))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil) 
  (:form-template (path) (html:div :|class| "form-group" 
                          (html:label :|class| "center-block"
                                      (synth :template label)
                                      (html:input ;; :|type| "text"
                                       :|class| "form-control" 
                                       :|formControlName| (string-downcase name)))))
  (:template () (html:div :|class| "form-group" 
                          (html:input 
                           :|type| "text"
                           :|id| (string-downcase name) 
                           :|placeholder| (if init (synth :doc init) ""))))

  (:controller () (ng-empty))
  (:form-controller (path) (ng-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ng-const "")))

