(in-package :gui)

(defprim description% (name source bindings)
  (:pretty () (list 'description (list :name name 
                                       :source (synth :pretty source) 
                                       :bindings (synth-plist :pretty bindings))))
  ;; (:req (path)
  ;;       (html:div
  ;;        (doc:text "Tabella denominata ")
  ;;        (html:span-color (string-downcase name))
  ;;        (doc:text " associata a ")
  ;;        (html:span-color (string-downcase (synth :name source)))
  ;;        (doc:text "(istanza del formato dati ")
  ;;        (html:a :href (concatenate 'string "#" (synth :string (synth :brief (synth :schema source)) 0))
  ;;                (html:code (synth :brief (synth :schema source)))) 
  ;;        (doc:text "con la seguente espressione:") 
  ;;        (html:p (synth :req bindings path))))
  ;; (:brief (path) (synth :req this path))
  ;; (:reqlist (*) nil)

 
  (:template () (apply #'html:div 
                       (mapcar (lambda (binding) 
                                 (list (html:label (doc:text "~a" (symbol-name (car binding)))) 
                                       (html:label (synth :template (cadr binding))))) 
                               (group bindings 2))))

  (:controller () (bb-empty))
  (:components (*) nil)
  (:routes (father) nil)
  (:imports () nil ;; (cons (bb-import (synth :name source) (synth :name source))
            ;;       nil
            ;;       ;; (apply #'append 
            ;;       ;;        (synth-plist-merge (lambda (pair)
            ;;       ;;                             (synth :imports (cadr pair)))
            ;;       ;;                           bindings))
            ;;       )
            )
  (:dependencies () nil))

(defmacro description (name source &body bindings)
  `(description% ,name
           ,source 
           (list ,@bindings)))

