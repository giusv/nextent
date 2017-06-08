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
  (:form-template (loopvar indexes) (html:div :|class| "form-group" 
                          (html:label :|class| "center-block"
                                      (synth :template label)
                                      (html:input ;; :|type| "text"
                                       :|class| "form-control" 
                                       :|formControlName| (lower-camel name)))))
  (:template () (html:div :|class| "form-group" 
                          (html:input 
                           :|type| "text"
                           :|id| (string-downcase name) 
                           :|placeholder| (if init (synth :doc init) ""))))

  (:controller () (bb-empty))
  (:form-controller (path) (bb-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (bb-new 'form-control (bb-const "")))
  (:imports () nil)
  (:dependencies () nil))

