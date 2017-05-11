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
  (:controller () (ng-empty)) 
  (:components (father) 
               (let ((unit-name (if father 
                                    (symb father "-" name)
                                    name))) 
                 ;; (pprint (synth :pretty (synth :template element)))
                 (cons (ng-unit unit-name
                                (ng-import "@angular/core" 'component 'on-init)
                                (ng-import "@angular/router" 'router 'activated-route 'params)
                                (synth :imports this)
                                (ng-primitive 'component
                                              :selector (ng-const (string-downcase name))
                                              :template (ng-template (synth :template element)))
                                (ng-class (mkstr unit-name "-component")
                                          :interfaces (list 'on-init)
                                          :constructor (ng-constructor 
                                                        (apply #'list 
                                                               (ng-pair 'route (ng-type 'activated-route) :private t)
                                                               (ng-pair 'router (ng-type 'router) :private t) 
                                                               (synth :dependencies this)))
                                          :fields (list (synth :controller element))
                                          :methods (list (ng-method (doc:text "~a" (doc:lower-camel 'ng-on-init)) nil (ng-type 'void :primitive t)))))
                       (synth :components element name))))
  (:routes (father) 
           (list (ng-object :path (ng-const (string-downcase (mkstr ":" param)))
                            :component (ng-static (mkstr father "-" name "-component"))
                            (aif (synth :routes element name)
                                 (list :children (ng-array it))))))
  (:imports () (synth :imports element))
  (:dependencies () (synth :dependencies element)))

(defmacro dynamic (name (param) element)
  `(let ((,param (url:path-parameter ',param)))
     (dynamic% ,name ',param ,element)))
