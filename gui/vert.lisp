(in-package :gui)
(defprim vert (&rest elements)
  (:pretty () (list 'vert (list :elements (synth-all :pretty elements))))
  (:req (path) (html:taglist (doc:text "Tale elemento si presenta come concatenazione verticale")
                        (if (not  elements) (doc:text "vuota") (doc:text "dei seguenti sottoelementi:"))
                        (apply #'html:ul (mapcar #'html:li (synth-all :req elements path)))))
  (:brief (path) (synth :req (apply #'vert elements) path))
  (:reqlist (path) (apply #'append (synth-all :req elements path)))
  (:template (&optional father) (apply #'taglist (synth-all :template elements father))))

(defmacro vert* (&rest elements) 
    `(let* ,elements
            (vert ,@(mapcar #'car elements))))
