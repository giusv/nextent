(in-package :gui)

(defprim listing% (name source rowname element)
  (:pretty () (list 'listing (list :name name 
                                   :source (synth :pretty source) 
                                   :rowname rowname
                                   :element (synth :pretty element))))
  (:req (path)
        (html:div
         (doc:text "Lista denominata ")
         (html:span-color (string-downcase name))
         (doc:text " associata a ")
         (html:span-color (string-downcase (synth :name source)))
         (doc:text "(istanza del formato dati ")
         (html:a :href (concatenate 'string "#" (synth :string (synth :brief (synth :schema source)) 0))
                 (html:code (synth :brief (synth :schema source)))) 
         (doc:text "con la seguente espressione:") 
         (html:p (synth :req element path))))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil)
  (:template () (progn
                  ;; (pprint "in listing%")
                  ;; (pprint (synth :pretty element))
                  (html:div :|*ngFor| (doc:hcat (doc:text "let ~a of ~a" (lower-camel rowname) (lower-camel (synth :name source))) )
                            (synth :template element))))

  (:controller () (bb-empty))
  (:components (*) nil)
  (:routes (father) nil)
  (:imports () (synth :imports element))
  (:dependencies () (synth :dependencies element)))

;; (defun csplice (cond &rest exps)
;;   (if cond
;;       `(,@exps)))


(defmacro listing (name source (rowname) &body element)
  `(listing% ,name
             ,source
             ',rowname
             (let ((,rowname ',rowname))
               ,@element)))

;; (defmacro listing* (source (rowname &optional index) &body element)
;;   `(listing% (gensym "LISTING") 
;;             ,source
;;             (lambda (,rowname ,@(csplice index index)) (declare (ignorable ,rowname ,@(csplice index index))) ,@element)))

