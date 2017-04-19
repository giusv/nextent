(in-package :angular)

 (defmacro within-braces (before inside)
     `(vcat (hcat ,before (text "{"))
            (nest 2 ,inside)
            (text "}")))
(defmacro within-parens (before inside)
  `(vcat (hcat ,before (text "("))
         (nest 2 ,inside)
         (text ")")))
;; (within-braces (text "@Component") (vcat (text "selector: ~a" selector)))
(defprim pair (name type &key init const)
  (:pretty () `(pair (:name ,name :type ,type :init ,(synth :pretty init) :const ,const)))
  (:typescript () (hcat (if const (text "const ") (empty))
                  (text "~a: ~a" (lower name) (lower type))
                  (if init 
                      (hcat (text " = ") (synth :typescript init))
                      (empty)))))

;; (defprim ng-array (&rest (elems (list primitive)))
;;   (:pretty () `(ng-array (:elems ,(synth-all :pretty elems))))
;;   (:typescript () (brackets (apply #'punctuate (comma) t (synth-all :typescript elems)) :padding 1 :newline nil)))

;; (defprod primitive (ng-primitive ((name symbol)
;;                                   &rest (props (plist primitive))))
;;   (:pretty () `(ng-primitive (name ,name :props ,(synth-plist :pretty props))))
;;   (:typescript () (vcat (text "@~a" (upper-camel name)) 
;;                   (parens
;;                    (braces 
;;                     (nest 4 (apply #'punctuate (comma) t 
;;                                    (synth-plist-merge 
;;                                     #'(lambda (pair) (hcat (text "~a: " (lower (first pair)))
;;                                                            (synth :typescript (second pair)))) 
;;                                     props)))
;;                     :newline t)))

;;          ;; (within-parens (text "@Component")
;;          ;;                (within-braces (empty)
;;          ;;                               (vcat (synth :typescript (descriptor 'selector selector))
;;          ;;                                     (synth :typescript (descriptor 'template-url template-url))
;;          ;;                                     (synth :typescript (descriptor 'style-urls (vector ))))))
;;          ))

;; (defprod primitive (ng-class ((name symbol)
;;                               &key (interfaces (interfaces (list symbol)))
;;                               (fields (fields (list ng-pair)))
;;                               (constructor (constructor constructor))
;;                               (methods (methods (list method)))))
;;   (:pretty () `(ng-class (:name ,name :interfaces (,@interfaces) 
;;                                 :fields ,(synth-all :pretty fields) 
;;                                 :constructor ,(synth :pretty constructor)
;;                                 :methods ,(synth-all :pretty methods))))
;;   (:typescript () (vcat (text "export class ~a" (lower name)) 
;;                   (braces 
;;                    (nest 4 (apply #'vcat (apply #'postpend (semi) t 
;;                                                 (synth-all :typescript fields))
;;                                   (synth :typescript constructor)
;;                                   (synth-all :typescript methods)))
;;                    :newline t))))



;; (defprod primitive (ng-method ((name symbol)
;;                                (parameters (list ng-pair))
;;                                (rtype symbol)
;;                                &rest (statements (list ng-statement))))
;;   (:pretty () `(ng-method (:name ,name 
;;                                   :parameters ,(synth-all :pretty parameters) 
;;                                   :rtype ,rtype
;;                                   :statements ,(synth-all :pretty statements))))
;;   (:typescript () (vcat (hcat (textify name) 
;;                         (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
;;                         (text ": ~a" (lower rtype))) 
;;                   (braces 
;;                    (nest 4 (apply #'postpend (semi) t 
;;                                   (synth-all :typescript statements)))
;;                    :newline t))))

;; (defprod primitive (ng-import ((name expression)
;;                                &rest (elements (list symbol))))
;;   (:pretty () `(ng-new (:name ,(synth :pretty name) 
;;                               :elements ,elements)))
;;   (:typescript () (hcat (text "import ")
;;                   (if elements (hcat (braces (apply #'punctuate (comma) nil (mapcar #'text (mapcar #'upper-camel elements))) :padding 1)
;;                                      (text " from ")
;;                                      (synth :typescript name))))))

;; (defprod primitive (ng-new ((name symbol)
;;                             &rest (parameters (list expression))))
;;   (:pretty () `(ng-new (:name ,name 
;;                               :parameters ,(synth-all :pretty parameters))))
;;   (:typescript () (hcat (text "new ~a" (upper name)) 
;;                   (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

;; (defprod primitive (ng-call ((name symbol)
;;                              &rest (parameters (list expression))))
;;   (:pretty () `(ng-call (:name ,name 
;;                                :parameters ,(synth-all :pretty parameters))))
;;   (:typescript () (hcat (textify name) 
;;                   (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

;; (defprod statement (ng-chain (&rest (calls (list ng-call))))
;;   (:pretty () `(ng-chain (:calls ,(synth :pretty calls))))
;;   (:typescript () (let ((calls (synth-all :typescript calls)))
;;               (hcat (car calls)
;;                     (apply #'prepend (dot) t (cdr calls))))))

;; (defprod primitive (ng-constructor ((parameters (list ng-pair))
;;                                     &rest (statements (list ng-statement))))
;;   (:pretty () `(ng-constructor (:parameters ,(synth-all :pretty parameters) 
;;                                             :statements ,(synth-all :pretty statements))))
;;   (:typescript () (vcat (hcat (text "constructor") 
;;                         (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))) 
;;                   (braces 
;;                    (nest 4 (apply #'postpend (semi) t 
;;                                   (synth-all :typescript statements)))
;;                    :newline t))))

;; (defprod primitive (ng-arrow ((parameters (list ng-pair))
;;                               &rest (statements (list ng-statement))))
;;   (:pretty () `(ng-arrow (:parameters ,(synth-all :pretty parameters) 
;;                                       :statements ,(synth-all :pretty statements))))
;;   (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
;;                   (text " => ") 
;;                   (braces 
;;                    (nest 4 (apply #'postpend (semi) t 
;;                                   (synth-all :typescript statements)))
;;                    :newline t))))


;; (defprod primitive (ng-unit (&rest (elements (list primitive))))
;;   (:pretty () `(ng-unit (:elements ,(synth :pretty elements))))
;;   (:typescript () (apply #'vcat (synth-all :typescript elements))))


;; (pprint (synth :string (synth :typescript (ng-primitive 'ng-module 
;;                                                     :selector (const "my-heroes") 
;;                                                     :template-url  (const "test") 
;;                                                     :style-urls (ng-array (const "test")))) 0))

;; (synth output (synth :typescript (ng-unit
;;                             (ng-import (const "@angular/core") 'component 'onInit)
;;                             (ng-primitive 'component 
;;                                           :selector (const "my-heroes") 
;;                                           :template-url  (const "test") 
;;                                           :style-urls (ng-array (const "test")))
;;                             (ng-class 'hero-search 
;;                                       :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                                                     (ng-pair 'heroes 'string :init (ng-array (const "aaa")))
;;                                                     (ng-pair 'heroes2 'string :init (ng-call 'get (const "aaa")))
;;                                                     (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (const "aaa"))
;;                                                                                         (ng-call 'set (const "aaa"))
;;                                                                                         (ng-call 'set (const "bbb"))))
;;                                                     (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (const 'e)))) :const t))
;;                                       :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                                       :methods (list (ng-method 'on-init 
;;                                                                 (list (ng-pair 'heroes 'string))
;;                                                                 'void))))) 0)

