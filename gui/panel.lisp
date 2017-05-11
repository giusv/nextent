(in-package :gui)
(defprim panel (name header body &optional footer)
  (:pretty () (list 'panel (:name name :header (synth :pretty header) :body (synth :pretty body) :footer (synth :pretty footer))))
  ;; (:req (path) (multitags 
  ;;                (text "Pannello identificato come")
  ;;                (span-color (lower-camel name)) 
  ;;                (text "e composto da:")
  ;;                (dlist header (span nil (text "header")) (synth :req header path)
  ;;                       body (span nil (text "body")) (synth :req body path)
  ;;                       footer (span nil (text "footer")) (synth :req footer path))))
  ;; (:brief (path) (synth :req (panel header body footer) path))
  ;; (:reqlist (*) nil)

  
  (:template () (html:div 
                 :|class| "panel panel-primary" 
                 (html:div
                  :|class| "panel-heading" (synth :template header))
                 (html:div
                  :|class| "panel-body" (synth :template body))
                 (if footer 
                     (html:div
                      :|class| "panel-footer" (synth :template footer)))))
  (:controller () (bb-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:imports () (append (synth :imports header)
                       (synth :imports body)
                       (if footer (synth :imports footer))))
  (:dependencies () (append (synth :dependencies header)
                       (synth :dependencies body)
                       (if footer (synth :dependencies footer)))))

(defmacro panel* (header body &optional footer)
  `(panel (gensym "PANEL") ,header ,body ,footer))
