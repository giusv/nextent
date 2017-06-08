(in-package :gui)

(defmacro evnames (&rest actions)
  `(append ,@(mapcar #'(lambda (action) 
                         `(if ,action (list ,(keyw "(" action ")") (concatenate 'string (lower-camel ',action) (upper-camel name) "()"))))
                     actions)))


(defprim button (name expr &key click)
  (:pretty () (list 'button (list :name name 
                                  :expr (synth :pretty expr)
                                  :click (synth :pretty click))))
  (:req (*) (html:taglist 
             (doc:text "Pulsante identificato come ")
             (html:span-color (string-downcase name))
             (doc:text " e etichettato con la seguente espressione:") 
             (synth :req expr)
             ;; (dlist click (span nil (doc:text "Sottoposto a click: ")) (synth :req click)
             ;;        hover (span nil (doc:text "Sottoposto a hover: ")) (synth :req hover))
             ))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil) 
  (:template () (html:button :|(click)| (doc:text "~aClick()" (lower-camel name))
                                        expr))

  (:controller () (lang:bb-method (doc:text "~aClick" (lower-camel name)) 
                                 nil
                                 (bb-type :void)))
  (:components (*) nil)
  (:routes (father) nil)
  (:imports () nil)
  (:dependencies () nil))
 

