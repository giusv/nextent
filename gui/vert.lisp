(in-package :gui)
(defprim vert (&rest elements)
  (:pretty () (list 'vert (list :elements (synth-all :pretty elements))))
  (:req (path) (html:taglist (doc:text "Tale elemento si presenta come concatenazione verticale")
                             (if (not  elements) (doc:text "vuota") (doc:text "dei seguenti sottoelementi:"))
                             (html:ul (mapcar #'html:li (synth-all :req elements path)))))
  (:brief (path) (synth :req this path))
  (:reqlist (path) (apply #'append (synth-all :req elements path)))
  (:template () (html:taglist (mapcar (lambda (template)
                                        (html:div
                                         :|class| "row"
                                         template))
                                      (synth-all :template elements))))
  (:controller () (ng-list (synth-all :controller elements))) 
  (:components (father) (apply #'append (synth-all :components elements father)))
  (:routes (father) (apply #'append (synth-all :routes elements father))))

(defmacro vert* (&rest elements) 
  (let ((gensyms (loop for v in elements collect (gensym (symbol-name (car v))))))
    `(let* ,(mapcar #2`(,a1 ,(cadr a2)) gensyms elements)
       (vert ,@gensyms))))
