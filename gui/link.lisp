(in-package :gui)
(defprim link (name expr target)
  (:pretty () (list 'link (list :name name 
                                :expr (synth :pretty expr) 
                                :target (synth :pretty target))))
  ;; (:html (path) (multitags 
  ;;                (text "Link identificato come ")
  ;;                (span-color (lower-camel name))
  ;;                (text " e etichettato con la seguente espressione:") 
  ;;                (synth :html expr)
  ;;                (dlist click (span nil (text "Sottoposto a click: ")) (synth :html click)
  ;;                       hover (span nil (text "Sottoposto a hover: ")) (synth :html hover))))
  (:brief (path) (synth :html (link name expr :click click :hover hover) path)) 
  (:reqlist (*) nil)
  (:template () (html:a 
                    :|routerLink| (synth :url target)
                    :|routerLinkActive| "active"
                    (synth :template expr)))

  (:controller () (bb-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:imports () nil)
  (:dependencies () nil))


