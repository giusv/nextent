(defprim navbar (name &rest anchors)
  (:pretty () (list 'navbar (list :name name :anchors (synth-all :pretty anchors))))
  ;; (:req (path) (funcall #'vcat 
  ;;       		  (text "Barra di navigazione con i seguenti elementi:")
  ;;       		  (nest 4 (apply #'vcat (synth-all :req anchors path)))))
  (:html (path) (multitags (text "Barra di navigazione ")
                             (if (not  anchors) 
                                 (text "vuota") 
                                 (text "composta dai seguenti link:"))
                             (apply #'ul nil ;; (list :class 'list-group)
                                    (mapcar #'listify (synth-all :html anchors path)))))
  (:brief (path) (synth :html (apply #'navbar name anchors) path))
  (toplevel () nil)
  (req (path) nil))

