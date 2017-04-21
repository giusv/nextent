(in-package :gui)
(defprim horz (&rest elements)
  (:pretty () (list 'horz (list :elements (synth-all :pretty elements))))
  (:req (path) (html:taglist (doc:text "Tale elemento si presenta come concatenazione horzicale")
                        (if (not  elements) (doc:text "vuota") (doc:text "dei seguenti sottoelementi:"))
                        (apply #'html:ul (mapcar #'html:li (synth-all :req elements path)))))
  (:brief (path) (synth :req this path))
  (:reqlist (path) (apply #'append (synth-all :req elements path)))
  (:template (&optional father) (apply #'html:taglist (synth-all :template elements father))))

(defmacro horz* (&rest elements) 
  (let ((gensyms (loop for v in elements collect (gensym (symbol-name (car v))))))
    `(let* ,(mapcar #2`(,a1 ,(cadr a2)) gensyms elements)
       (horz ,@gensyms))))