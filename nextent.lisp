;;;; nextent.lisp

(in-package :nextent)

;; (synth :output (synth :typescript (synth :controller (gui:input 'invio (text "name")))) 0)
;; (synth :output (synth :doc (synth :template (gui:button 'vai (text "vaiClick")))) 0)

;; (synth :output (synth :doc (synth :template (gui:horz*
;;                                              (vai (gui:static :test nil (gui:button 'vai (text "vaiClick"))))
;;                                              (torna (gui:button 'torna (text "tornaClick")))))) 0)

(defparameter gui (gui:static 'app nil (gui:vert (gui:button 'here (text "here!"))
                                                 (gui:button 'there (text "there!"))
                                                 (gui:input 'write (text "write!")))))
(let ((output (synth :string (synth :doc (apply #'vcat (synth-all :typescript (synth :components gui))))))
      (filename (lol::mkstr "d:/giusv/angular/template/src/app/" (lower-camel (synth :name gui)) ".component.ts"))) 
  (pprint filename)
  (pprint output)
  (write-file filename (synth :string (synth :doc (apply #'vcat (synth-all :typescript (synth :components gui)))))))

;; (synth :output (synth :typescript (ng-unit (ng-import (ng-const "@angular/core") 'component 'onInit)
;;                                            (ng-primitive 'component 
;;                                                          :selector (ng-const "my-heroes") 
;;                                                          :template-url  (ng-const "test") 
;;                                                          :style-urls (ng-array (ng-const "test")))
;;                                            (ng-class 'hero-search 
;;                                                      :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                                                                    (ng-pair 'heroes 'string :init (ng-array (ng-const "aaa")))
;;                                                                    (ng-pair 'heroes2 'string :init (ng-call 'get (ng-const "aaa")))
;;                                                                    (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (ng-const "aaa"))
;;                                                                                                              (ng-call 'set (ng-const "aaa"))
;;                                                                                                              (ng-call 'set (ng-const "bbb"))))
;;                                                                    (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (ng-const 'e)))) :const t))
;;                                                      :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                                                      :methods (list (ng-method (text "on-init") 
;;                                                                                (list (ng-pair 'heroes 'string))
;;                                                                                'void))))) 0)

 ;; (synth :output (nest 10 (ng-const "~a" 24)) 0)
;; (pprint (synth :output (synth :doc (html:div :class "a" (doc:ng-const "SS"))) 0))
;; (pprint (synth :pretty (html:div :class "a" (ng-const "ss"))))
;; (synth :output (synth :doc (synth :template (gui:input 'name (ng-const "Name") :init (ng-const "hello")))) 0)

;; (pprint (synth :pretty (ng-unit
;;                         (ng-import (ng-const "@angular/core") 'component 'onInit)
;;                         (ng-primitive 'component 
;;                                       :selector (ng-const "my-heroes") 
;;                                       :template-url  (ng-const "test") 
;;                                       :style-urls (ng-array (ng-const "test")))
;;                         (ng-class 'hero-search 
;;                                   :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                                                 (ng-pair 'heroes 'string :init (ng-array (ng-const "aaa")))
;;                                                 (ng-pair 'heroes2 'string :init (ng-call 'get (ng-const "aaa")))
;;                                                 (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (ng-const "aaa"))
;;                                                                                           (ng-call 'set (ng-const "aaa"))
;;                                                                                           (ng-call 'set (ng-const "bbb"))))
;;                                                 (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (ng-const 'e)))) :const t))
;;                                   :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                                   :methods (list (ng-method 'on-init 
;;                                                             (list (ng-pair 'heroes 'string))
;;                                                             'void))))))

;; (synth :typescript (ng-pair 'e 'error))
;; (synth :typescript (ng-unit
;;                     (ng-import (text "@angular/core") 'component 'onInit)
;;                     ;; (ng-primitive 'component 
;;                     ;;               :selector (text "my-heroes") 
;;                     ;;               :template-url  (text "test") 
;;                     ;;               :style-urls (ng-array (text "test")))
;;                     ;; (ng-class 'hero-search 
;;                     ;;           :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                     ;;                         (ng-pair 'heroes 'string :init (ng-array (text "aaa")))
;;                     ;;                         (ng-pair 'heroes2 'string :init (ng-call 'get (text "aaa")))
;;                     ;;                         (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (text "aaa"))
;;                     ;;                                                                   (ng-call 'set (text "aaa"))
;;                     ;;                                                                   (ng-call 'set (text "bbb"))))
;;                     ;;                         (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (text 'e)))) :const t))
;;                     ;;           :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                     ;;           :methods (list (ng-method 'on-init 
;;                     ;;                                     (list (ng-pair 'heroes 'string))
;;                     ;;                                     'void)))
;;                     ))

;; (pprint (synth :pretty (div)))

;; (synth :output (html:div :class "a" (text "aaa")))
;; (pprint (parse (many (atomic)) '(a b c &optional d)))


;; (let ((l '(a b c &optional d1 (d2 0 d2-supplied-p) &rest e &key f (g 99 g-supplied-p))))
;;   ;; (pprint (parse (lambda-list) l))
;;   (pprint (arg-names l))
;;   ;; (let ((args (parse (lambda-list) l)))
;;   ;;   (pprint (apply #'append (mapcar (lambda (x) (getf args x))  (list :req :opt :rest :key))))
;;   ;;   )
;;   )
;; (pprint (parse (ttt) (list 'a 'b)))
;; (pprint (parse (ttt) '(a b)))
;; (pprint (parse (var-init) '((a b))))


;; (pprint (parse (var-init) '((name init))))
;;; "nextent" goes here. Hacks and glory await!

