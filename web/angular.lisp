(in-package :web)

;;  (defmacro within-braces (before inside)
;;      `(vcat (hcat ,before (text "{"))
;;             (nest 2 ,inside)
;;             (text "}")))
;; (defmacro within-parens (before inside)
;;   `(vcat (hcat ,before (text "("))
;;          (nest 2 ,inside)
;;          (text ")")))
;; (within-braces (text "@Component") (vcat (text "selector: ~a" selector)))

(defprim ng-pair (name type &key init const)
  (:pretty () `(ng-pair (:name ,name :type ,type :init ,(synth :pretty init) :const ,const)))
  (:typescript () (hcat (if const (text "const ") (empty))
                        (text "~a: ~a" (string-downcase name) (string-downcase type))
                        (if init 
                            (hcat (text " = ") (synth :typescript init))
                            (empty)))))

(defprim ng-const (lit)
  (:pretty () (list 'ng-const (list :lit lit)))
  (:typescript () (double-quotes (text "~a" lit))))

(defprim ng-array (&rest elems)
  (:pretty () `(ng-array (:elems ,(synth-all :pretty elems))))
  (:typescript () (brackets (apply #'punctuate (comma) t (synth-all :typescript elems)) :padding 1 :newline nil)))

(defprim ng-primitive (name &rest props)
  (:pretty () `(ng-primitive (name ,name :props ,(synth-plist :pretty props))))
  (:typescript () (vcat (text "@~a" (upper-camel name)) 
                  (parens
                   (braces 
                    (nest 4 (apply #'punctuate (comma) t 
                                   (synth-plist-merge 
                                    #'(lambda (pair) (hcat (text "~a: " (string-downcase (first pair)))
                                                           (synth :typescript (second pair)))) 
                                    props)))
                    :newline t)))))

(defprim ng-class (name &key interfaces fields constructor methods)
  (:pretty () (list 'ng-class (list :name name 
                                    :interfaces interfaces 
                                    :fields (synth-all :pretty fields) 
                                    :constructor (synth :pretty constructor)
                                    :methods (synth-all :pretty methods))))
  (:typescript () (vcat (text "export class ~a" (string-downcase name)) 
                        (braces 
                         (nest 4 (apply #'vcat (apply #'postpend (semi) t 
                                                      (synth-all :typescript fields))
                                        (synth :typescript constructor)
                                        (synth-all :typescript methods)))
                         :newline t))))



(defprim ng-method (name parameters rtype &rest statements)
  (:pretty () (list 'ng-method (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype
                                     :statements (synth-all :pretty statements))))
  (:typescript () (vcat (hcat name
                              (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
                              (text ": ~a" (string-downcase rtype))) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t))))

(defprim ng-import (name &rest elements)
  (:pretty () (list 'ng-import (list :name (synth :pretty name) 
                                     :elements elements)))
  (:typescript () (if elements 
                      (hcat (text "import ")
                            (hcat (braces (apply #'punctuate (comma) nil (mapcar #'text (mapcar #'upper-camel elements))) :padding 1)
                                  (text " from ") (synth :typescript name))))))

(defprim ng-new (name &rest parameters)
  (:pretty () (list 'ng-new (list :name name 
                                  :parameters (synth-all :pretty parameters))))
  (:typescript () (hcat (text "new ~a" (upper-camel name)) 
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

(defprim ng-call (name &rest parameters)
  (:pretty () (list 'ng-call (list :name name 
                                   :parameters (synth-all :pretty parameters))))
  (:typescript () (hcat (text "~a" (lower-camel name))
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

(defprim ng-chain (&rest calls)
  (:pretty () (list 'ng-chain (list :calls (synth-all :pretty calls))))
  (:typescript () (let ((calls (synth-all :typescript calls)))
                    (hcat (car calls)
                          (apply #'prepend (dot) t (cdr calls))))))

(defprim ng-constructor (parameters &rest statements)
  (:pretty () (list 'ng-constructor (list :parameters (synth-all :pretty parameters) 
                                          :statements (synth-all :pretty statements))))
  (:typescript () (vcat (hcat (text "constructor") 
                              (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t))))

(defprim ng-arrow (parameters &rest statements)
  (:pretty () (list 'ng-arrow (list :parameters (synth-all :pretty parameters) 
                                    :statements (synth-all :pretty statements))))
  (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
                        (text " => ") 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t))))


(defprim ng-unit (&rest elements)
  (:pretty () (list 'ng-unit (list :elements (synth-all :pretty elements))))
  (:typescript () (apply #'vcat (synth-all :typescript elements))))


