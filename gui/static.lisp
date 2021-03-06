(in-package :gui)

(defprim static% (name queries element)
  (:pretty () (list 'static (list :name name :queries queries :element (synth :pretty element))))
  (:brief (path) (let ((newpath (backward-chain (static-chunk name) path)))
                   (html:strong 
                    (doc:text "~a (URL: " (upper-camel name #\Space)) 
                    (html:a :href (doc:text "#~a" (synth :string (synth :url newpath) 0))
                            (html:code (synth :url newpath)))
                    (doc:text ")"))))

  (:req (path) (let ((newpath (backward-chain (static-chunk name) path)))
                 (html:taglist 
                  (html:section 
                   (html:h4 nil (doc:text "~a (URL: " (upper-camel name #\Space)) 
                            (html:code :id (synth :string (synth :url newpath) 0) 
                                       (synth :url newpath))
                            (doc:text ")"))
                   (synth :req element newpath)))))
  (:reqlist (path) 
            (let ((newpath (backward-chain (static-chunk name) path))) 
              (cons (synth :req this path)
                    (synth :reqlist element newpath))))
  (:template () (html:tag name (doc:empty)))
  (:controller () (bb-empty)) 
  (:components (father) 
               (let ((unit-name (if father 
                                    (symb father "-" name)
                                    name))) 
                 ;; (pprint (synth :pretty (synth :template element)))
                 (cons (bb-unit unit-name
                                (bb-import "@angular/core" 'component)
                                (synth :imports this)
                                (bb-annotation 'component
                                              :selector (bb-const (string-downcase name))
                                              :template (bb-template (synth :template element)))
                                (bb-class (mkstr unit-name "-component")
                                          :constructor (bb-constructor (synth :dependencies this))
                                          :fields (list (synth :controller element))))
                       (synth :components element name))))
  (:routes (father) 
           (list (bb-object :path (progn (pprint (string-downcase name)) (bb-const (string-downcase name)))
                            :component (bb-static (mkstr father "-" name "-component"))
                            (aif (synth :routes element name)
                                 (list :children (bb-array it))))))
  (:imports () (synth :imports element))
  (:dependencies () (synth :dependencies element)))

(defmacro static (name (&rest queries) element)
  `(let ,(mapcar #'(lambda (query) 
                     `(,query (url:query-parameter ',query)))
                 queries)
     (static% ,name (list ,queries) ,element)))
