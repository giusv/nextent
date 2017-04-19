(in-package :gui)

(defprim input (name label &key init)
  (:pretty () `(input (:name ,name :label ,(synth :pretty label) :init ,(synth :pretty init))))
  (:req (path) (html:taglist (doc:text "Campo di input identificato come")
                         ;; (span-color (string-downcase name))
                         (doc:text " etichettato con ")
                         (synth :req label) 
                         ;; (if init
                         ;;     (dlist init (span nil (text "Valore iniziale")) (synth :req init)))
                         ))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil) 
  (:template (&optional father) (html:input :type "text" 
                                            :class "form-control" 
                                            :id (string-downcase name) 
                                            :|[(ngModel)]| (if father 
                                                               (doc:text "~a.~a" father name)
                                                               (doc:text "~a" name))
                                            :name (string-downcase name)
                                            :placeholder (synth :doc init))))
 

