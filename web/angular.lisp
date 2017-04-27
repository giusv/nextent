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

(defprim ng-empty ()
  (:pretty () (list 'ng-empty))
  (:typescript () (empty)))
(defprim ng-pair (name type &key init const private)
  (:pretty () (list 'ng-pair (list :name name :type type :init (synth :pretty init) :const const)))
  (:typescript () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (text "~a: ~a" (lower-camel name) (upper-camel type))
                        (if init 
                            (hcat (text " = ") (synth :typescript init))
                            (empty)))))

(defprim ng-const (lit)
  (:pretty () (list 'ng-const (list :lit lit)))
  (:typescript () (single-quotes (if lit
                                     (text "~a" lit)
                                     (empty)))))

;; (defprim ng-bool (value)
;;   (:pretty () (list 'ng-bool (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim ng-number (value)
;;   (:pretty () (list 'ng-number (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim ng-string (value)
;;   (:pretty () (list 'ng-string (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

(defprim ng-array (&rest elems)
  (:pretty () (list 'ng-array (list :elems (synth-all :pretty elems))))
  (:typescript () (brackets (apply #'punctuate (comma) t (synth-all :typescript (apply #'append* elems))) :padding 1 :newline nil)))

(defprim ng-object (&rest elems)
  (:pretty () (list 'ng-object (list :elems (synth-all :pretty (apply #'append* elems)))))
  (:typescript () (braces 
                   (nest 4 (apply #'punctuate (comma) t 
                                  (synth-plist-merge 
                                   #'(lambda (pair) (hcat (text "~a: " (lower-camel (first pair)))
                                                          (synth :typescript (second pair)))) 
                                   (apply #'append* elems))))
                   :newline t)))
(defprim ng-template (element)
  (:pretty () (list 'ng-template (list :element (synth :pretty element))))
  (:typescript () (back-quotes (synth :doc element) :newline t)))


(defprim ng-primitive (name &rest props)
  (:pretty () (list 'ng-primitive (list name name :props (synth-plist :pretty props))))
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
  (:typescript () (vcat (text "export class ~a" (upper-camel name)) 
                        (braces 
                         (nest 4 (apply #'vcat (apply #'postpend (semi) t 
                                                      (synth-all :typescript fields))
                                        (synth :typescript constructor)
                                        (synth-all :typescript methods)))
                         :newline t))))

;; (defprim taglist (&rest tags)
;;   (:pretty () (list 'taglist (list :tags (synth-all :pretty tags))))
;;   (:doc () (apply #'doc:vcat (synth-all :doc (apply #'append* tags)))))



(defprim ng-list (&rest statements)
  (:pretty () (list 'ng-list (list :statements (synth-all :pretty (apply #'append* statements)))))
  (:typescript () (apply #'punctuate (semi) t (synth-all :typescript (apply #'append* statements)))))

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
                            (hcat (braces (apply #'punctuate (text ", ") nil (mapcar #'text (mapcar #'upper-camel elements))) :padding 1)
                                  (text " from ") (synth :typescript name) (semi))))))

(defprim ng-assign (lhs rhs &key as)
  (:pretty () (list 'ng-assign (list :lhs lhs :rhs rhs)))
  (:typescript () (hcat (synth :typescript lhs)
                        (text " = ")
                        (synth :typescript rhs)
                        (if as (text " as ~a" (doc:upper-camel as))))))

(defprim ng-new (name &rest parameters)
  (:pretty () (list 'ng-new (list :name name 
                                  :parameters (synth-all :pretty parameters))))
  (:typescript () (hcat (text "new ~a" (upper-camel name)) 
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

(defprim ng-call (name &rest args)
  (:pretty () (list 'ng-call (list :name name 
                                   :parameters (synth-all :pretty (rest-plain args))
                                   :as (getf (rest-key args) :as))))
  (:typescript () (hcat (text "~a" (lower-camel name))
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript (rest-plain args))))
                        (aif (getf (rest-key args) :as)
                             (text " as ~a" (doc:upper-camel it))
                             (empty)))))
(defprim ng-static (name)
  (:pretty () (list 'ng-static (list :name name)))
  (:typescript () (text "~a" (upper-camel name))))
(defprim ng-dynamic (name)
  (:pretty () (list 'ng-dynamic (list :name name)))
  (:typescript () (text "~a" (lower-camel name))))

(defprim ng-chain (&rest args)
  (:pretty () (list 'ng-chain (list :calls (synth-all :pretty (apply #'append* (rest-plain args)))
                                    :as (getf (rest-key args) :as))))
  (:typescript () (let ((calls (synth-all :typescript (apply #'append* (rest-plain args))))
                        (as (getf (rest-key args) :as)))
                    (hcat (car calls)
                          (apply #'prepend (dot) t (cdr calls))
                          (if as (text " as ~a" (doc:upper-camel as) (empty)))))))

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


(defprim ng-unit (name &rest elements)
  (:pretty () (list 'ng-unit (list :name name :elements (synth-all :pretty (apply #'append* elements)))))
  (:typescript () (apply #'vcat 
                         ;; (text "Unit ~a" name)
                         (synth-all :typescript (apply #'append* elements)))))


