(in-package :gui)

(defprim dynamic% (name param element)
  (:pretty () (list 'dynamic (list :name name :param param :element (synth :pretty element))))
  ;; (:brief (path) (let ((newpath (backward-chain (dynamic-chunk name) path)))
  ;;                  (html:strong 
  ;;                   (doc:text "~a (URL: " (upper-camel name #\Space)) 
  ;;                   (html:a :href (doc:text "#~a" (synth :string (synth :url newpath) 0))
  ;;                           (html:code (synth :url newpath)))
  ;;                   (doc:text ")"))))

  ;; (:req (path) (let ((newpath (backward-chain (dynamic-chunk name) path)))
  ;;                (html:taglist 
  ;;                 (html:section 
  ;;                  (html:h4 nil (doc:text "~a (URL: " (upper-camel name #\Space)) 
  ;;                           (html:code :id (synth :string (synth :url newpath) 0) 
  ;;                                      (synth :url newpath))
  ;;                           (doc:text ")"))
  ;;                  (synth :req element newpath)))))
  ;; (:reqlist (path) 
  ;;           (let ((newpath (backward-chain (dynamic-chunk name) path))) 
  ;;             (cons (synth :req this path)
  ;;                   (synth :reqlist element newpath))))
  (:template () (html:tag name (doc:empty)))
  (:controller () (bb-empty)) 
  (:components (father) 
               (let ((unit-name (if father 
                                    (symb father "-" name)
                                    name))) 
                 ;; (pprint (synth :pretty (synth :template element)))
                 (cons (bb-unit unit-name
                                (bb-import "@angular/core" 'component 'on-init)
                                (bb-import "@angular/router" 'router 'activated-route 'params)
                                (synth :imports this)
                                (bb-annotation 'component
                                              :selector (bb-const (string-downcase name))
                                              :template (bb-template (synth :template element)))
                                (bb-class (mkstr unit-name "-component")
                                          :interfaces (list 'on-init)
                                          :constructor (bb-constructor 
                                                        (apply #'list 
                                                               (bb-pair 'route (bb-type 'activated-route) :private t)
                                                               (bb-pair 'router (bb-type 'router) :private t) 
                                                               (synth :dependencies this)))
                                          :fields (list 
                                                   (bb-pair param (bb-type 'any :primitive t))
                                                   (synth :controller element))
                                          :methods (list (bb-method (doc:text "~a" (lower-camel 'bb-on-init))
                                                                    nil (bb-type :void)
                                                                    (bb-chain (bb-dynamic 'this)
                                                                              (bb-dynamic 'route)
                                                                              (bb-dynamic 'params)
                                                                              (bb-call 'subscribe 
                                                                                       (bb-arrow (list (bb-pair 'params (bb-type 'any :primitive t)))
                                                                                                 (bb-assign (bb-chain (bb-dynamic 'this) 
                                                                                                                      (bb-dynamic param))
                                                                                                            (bb-element 'params (bb-const "id"))))))))))
                       (synth :components element name))))
  (:routes (father) 
           (list (bb-object :path (bb-const (string-downcase (mkstr ":" param)))
                            :component (bb-static (mkstr father "-" name "-component"))
                            (aif (synth :routes element name)
                                 (list :children (bb-array it))))))
  (:imports () (synth :imports element))
  (:dependencies () (synth :dependencies element)))

(defmacro dynamic (name (param) element)
  `(let ((,param (url:path-parameter ',param)))
     (dynamic% ,name ',param ,element)))
