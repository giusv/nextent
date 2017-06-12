(in-package :lang)

;;  (defmacro within-braces (before inside)
;;      `(vcat (hcat ,before (text "{"))
;;             (nest 2 ,inside)
;;             (text "}")))
;; (defmacro within-parens (before inside)
;;   `(vcat (hcat ,before (text "("))
;;          (nest 2 ,inside)
;;          (text ")")))
;; (within-braces (text "@Component") (vcat (text "selector: ~a" selector)))

(defprim bb-empty ()
  (:pretty () (list 'bb-empty))
  (:typescript () (empty))
  (:java () (empty)))

(defprim bb-comment (text)
  (:pretty () (list 'bb-comment (list :text text)))
  (:typescript () (text "//~a" (synth :string text)))
  (:java () (text "//~a" (synth :string text))))

(defprim bb-pair (name type &key init const private)
  (:pretty () (list 'bb-pair (list :name name :type (synth :pretty type) :init (synth :pretty init) :const const)))
  (:typescript () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (text "~a: " (lower-camel name)) (synth :typescript type)
                        (if init 
                            (hcat (text " = ") (synth :typescript init))
                            (empty))))
  (:java () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (synth :java type) 
                        (text " ~a" (lower-camel name))
                        (if init 
                            (hcat (text " = ") (synth :java init))
                            (empty)))))




(defprim bb-const (lit)
  (:pretty () (list 'bb-const (list :lit lit)))
  (:typescript () (cond ((stringp lit) (single-quotes (text "~a" lit)))
                        ((numberp lit) (text "~a" lit))
                        ((symbolp lit) (text "~a" (lower-camel lit)))
                        (t (empty))))
  (:java () (cond ((stringp lit) (double-quotes (text "~a" lit)))
                  ((numberp lit) (text "~a" lit))
                  ((symbolp lit) (text "~a" (lower-camel lit)))
                  (t (empty)))))




(defprim bb-try (body catches &optional finally)
  (:pretty () (list 'bb-try (list :body (synth :pretty body) :catches (synth-plist :pretty catches))))
  (:typescript () (error "not implemented yet"))
  (:java () (apply #'vcat (text "try")
                   (braces 
                    (nest 4 (synth :java body))
                    :newline t)
                   (synth-all :java catches))))

(defprim bb-catch% (exceptions name body)
  (:pretty () (list 'bb-catch (list :exceptions exceptions :name name :body (synth :pretty body))))
  (:typescript () (error "not implemented yet"))
  (:java () (vcat (hcat+ (text "catch")
                         (parens (hcat+ (apply #'punctuate (text " | ") nil (mapcar (lambda (e)
                                                                       (textify (upper-camel e)))
                                                                     exceptions))
                                        (textify (lower-camel name))))) 
                  (braces 
                   (nest 4 (synth :java body))
                   :newline t))))

(defmacro bb-catch (exceptions body)
  `(let* ((,(car exceptions) ',(car exceptions))) 
     (bb-catch% (list ,@(cdr exceptions)) ,(car exceptions) ,body)))

(defprim bb-primitive-type (name)
  (:pretty () (list 'bb-primitive-type (list :name name)))
  (:typescript () (hcat (text "~a" (lower-camel name))))
  (:java () (hcat (text "~a" (lower-camel name)))))

(defprim bb-array-type (type)
  (:pretty () (list 'bb-array-type (list :type type)))
  (:typescript () (hcat (synth :typescript type) 
                        (text "[]")))
  (:java () (hcat (synth :java type) 
                  (text "[]"))))

(defprim bb-object-type (name)
  (:pretty () (list 'bb-object-type (list :name name)))
  (:typescript () (upper-camel name))
  (:java () (textify (upper-camel name))))

(defprim bb-type (name &key primitive array template)
  (:pretty () (list 'bb-type (list :name name :primitive primitive :array array :template template)))
  (:typescript () (hcat (text "~a" (if (or (eq name :string)
                                           (eq name :number)
                                           (eq name :any)
                                           (eq name :void)) 
                                       (lower-camel name)
                                       (upper-camel name)))
                        (if array (brackets (empty)) (empty))
                        (if template (angular (synth :typescript template)))))
  (:java () (hcat (text "~a" (if (or (eq name :int)
                                     (eq name :float)
                                     (eq name :double)
                                     (eq name :void)) 
                                 (lower-camel name)
                                 (upper-camel name)))
                  (if array (brackets (empty)) (empty))
                  (if template (angular (synth :java template))))))


;; (defprim bb-bool (value)
;;   (:pretty () (list 'bb-bool (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim bb-number (value)
;;   (:pretty () (list 'bb-number (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim bb-string (value)
;;   (:pretty () (list 'bb-string (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

(defprim bb-array (&rest elems)
  (:pretty () (list 'bb-array (list :elems (synth-all :pretty elems))))
  (:typescript () (brackets (apply #'punctuate (comma) t (synth-all :typescript (apply #'append* elems))) :padding 1 :newline nil))
  (:java () (brackets (apply #'punctuate (comma) t (synth-all :java (apply #'append* elems))) :padding 1 :newline nil)))

(defprim bb-object (&rest elems)
  (:pretty () (list 'bb-object (list :elems (synth-plist :pretty (apply #'append* elems)))))
  (:typescript () (braces 
                   (nest 4 (apply #'punctuate (comma) t 
                                  (synth-plist-merge 
                                   #'(lambda (pair) (hcat (text "~a: " (lower-camel (first pair)))
                                                          (synth :typescript (second pair)))) 
                                   (apply #'append* elems))))
                   :newline t))
  (:java () (error "not available in java")))

(defprim bb-template (element)
  (:pretty () (list 'bb-template (list :element (synth :pretty element))))
  (:typescript () (back-quotes (synth :doc element) :newline t))
  (:java () (error "not available in java")))

(defprim bb-with-annotations (annotations expr &key (newline t))
  (:pretty () (list 'bb-with-annotation (list :annotations (synth-all :pretty annotations) :expr (synth :pretty expr))))
  (:typescript () (apply (if newline #'vcat #'hcat) (append* (synth-all :typescript annotations)
                                                                 (synth :typescript expr))))
  (:java () (apply (if newline #'vcat #'hcat+) (append* (synth-all :java annotations)
                                                           (synth :java expr)))))

(defprim bb-annotation (name &rest props)
  (:pretty () (list 'bb-annotation (list name name :props (synth-plist :pretty props))))
  (:typescript () (vcat (text "@~a" (upper-camel name)) 
                        (parens
                         (braces 
                          (nest 4 (apply #'punctuate (comma) nil
                                         (synth-plist-merge 
                                          #'(lambda (pair) (hcat (text "~a: " (string-downcase (first pair)))
                                                                 (synth :typescript (second pair)))) 
                                          props)))))))
  (:java () (hcat (text "@~a" name) 
                  (cond ((null props) (empty))
                        ((plist-p props) (parens
                                          (apply #'punctuate (comma) nil 
                                                 (synth-plist-merge 
                                                  #'(lambda (pair) (hcat (text "~a = " (first pair))
                                                                         (second pair))) 
                                                  props))))
                        ((and (listp props) (= (length props) 1)) (parens (car props)))
                        (listp props) (parens (braces (punctuate (comma) nil props)))
                        (t (error "case not allowed"))))))

(defprim bb-class (name &key public interfaces parent fields constructor methods)
  (:pretty () (list 'bb-class (list :name name 
                                    :public public
                                    :interfaces interfaces 
                                    :parent parent 
                                    :fields (synth-all :pretty fields) 
                                    :constructor (synth :pretty constructor)
                                    :methods (synth-all :pretty methods))))
  (:typescript () (vcat (hcat (text "export class ~a" (upper-camel name))
                              (if interfaces (hcat (text " implements ") 
                                                   (punctuate (comma) nil (mapcar (lambda (int) (text "~a" (upper-camel int)))
                                                                                  interfaces))))) 
                        (braces 
                         (nest 4 (apply #'vcat (apply #'postpend (semi) t 
                                                      (synth-all :typescript fields))
                                        (synth :typescript constructor)
                                        (synth-all :typescript methods)))
                         :newline t)))
  (:java () (vcat (hcat 
                   (if public (text "public ") (empty)) 
                   (text "class ~a" (upper-camel name))
                   (if parent (hcat (text " extends ") (text "~a" (upper-camel parent))))
                   (if interfaces (hcat (text " implements ") 
                                        (punctuate (comma) nil (mapcar (lambda (int) (text "~a" (upper-camel int)))
                                                                       interfaces))))) 
                  (braces 
                   (nest 4 (apply #'vcat 
                                  (append* 
                                   (synth-all :java fields)
                                   (synth :java constructor)
                                   (synth-all :java methods))))
                   :newline t))))

(defprim bb-interface (name &key public interfaces methods)
  (:pretty () (list 'bb-interface (list :name name 
                                        :public public
                                        :interfaces interfaces 
                                        :methods (synth-all :pretty methods))))
  (:typescript () (error "not implemented yet"))
  (:java () (vcat (hcat
                   (if public (text "public ") (empty)) 
                   (text "interface ~a" (upper-camel name))) 
                  (braces 
                   (nest 4 (apply #'vcat (synth-all :java methods)))
                   :newline t))))

;; (defprim taglist (&rest tags)
;;   (:pretty () (list 'taglist (list :tags (synth-all :pretty tags))))
;;   (:doc () (apply #'doc:vcat (synth-all :doc (apply #'append* tags)))))



(defprim bb-statement (expression)
  (:pretty () (list 'bb-statement (list :expression expression)))
  (:typescript () (hcat (synth :typescript expression) (semi)))
  (:java () (hcat (synth :java expression) (semi))))

(defprim bb-list (&rest statements)
  (:pretty () (list 'bb-list (list :statements (synth-all :pretty (apply #'append* statements)))))
  (:typescript () (apply #'punctuate (semi) t (synth-all :typescript (apply #'append* statements))))
  (:java () (apply #'vcat (synth-all :java (apply #'append* statements)))))

(defprim bb-method (name parameters rtype &rest statements)
  (:pretty () (list 'bb-method (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype
                                     :statements (synth-all :pretty statements))))
  (:typescript () (vcat (hcat name
                              (parens (apply #'punctuate (comma) t (synth-all :typescript parameters)))
                              (text ": ") 
                              (synth :typescript rtype)) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t)))
  (:java ()  (vcat (hcat (text "public ") 
                        (synth :java rtype)
                        (blank)
                        name
                        (parens (apply #'punctuate (comma) t (synth-all :java parameters)))) 
                  (braces 
                   (nest 4 (apply #'vcat (synth-all :java statements)))
                   :newline t))))

(defprim bb-signature (name parameters rtype)
  (:pretty () (list 'bb-signature (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype)))
  (:typescript () (error "not implemented"))
  (:java ()  (hcat (text "public ") 
                  (synth :java rtype)
                  (blank)
                  name
                  (parens (apply #'punctuate (comma) t (synth-all :java parameters))) 
                  (semi))))

(defprim bb-import (name &rest elements)
  (:pretty () (list 'bb-import (list :name (synth :pretty name) 
                                     :elements elements)))
  (:typescript () (hcat (text "import ")
                        (if elements 
                            (hcat (braces (apply #'punctuate (text ", ") nil (mapcar #'text (mapcar #'upper-camel elements))) :padding 1)
                                  (text " from "))
                            (empty)) 
                        (text "'~a'" name)
                        (semi)))
  (:java () (mapcar (lambda (elem)
                      (hcat (text "import ~a.~a" name elem)
                            (semi)))
                    elements)))

(defprim bb-package (name)
  (:pretty () (list 'bb-package (list :name name)))
  (:typescript () (error "not foreseen"))
  (:java () (text "package ~a;" name)))

(defprim bb-assign (lhs rhs &key as)
  (:pretty () (list 'bb-assign (list :lhs lhs :rhs rhs)))
  (:typescript () (hcat (synth :typescript lhs)
                        (text " = ")
                        (synth :typescript rhs)
                        (if as (text " as ~a" (upper-camel as)))))
  (:java () (hcat (synth :java lhs)
                        (text " = ")
                        (if as (parens (text "~a" (upper-camel as))))
                        (synth :java rhs))))

(defprim bb-new (name &rest parameters)
  (:pretty () (list 'bb-new (list :name name 
                                  :parameters (synth-all :pretty parameters))))
  (:typescript () (hcat (text "new ~a" (upper-camel name)) 
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))))
  (:java () (hcat (text "new ~a" (upper-camel name)) 
                        (parens (apply #'punctuate (comma) nil (synth-all :java parameters))))))

(defprim bb-call (name &rest args)
  (:pretty () (list 'bb-call (list :name name 
                                   :parameters (synth-all :pretty (rest-plain args))
                                   :as (getf (rest-key args) :as))))
  (:typescript () (hcat (text "~a" (lower-camel name))
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript (rest-plain args))))
                        (aif (getf (rest-key args) :as)
                             (text " as ~a" (upper-camel it))
                             (empty))))
  (:java () (hcat (aif (getf (rest-key args) :as)
                       (hcat (parens (text "~a" (upper-camel as))) (blank))
                       (empty))
                  (text "~a" (lower-camel name))
                  (parens (apply #'punctuate (comma) nil (synth-all :java (rest-plain args)))))))
(defprim bb-static (name)
  (:pretty () (list 'bb-static (list :name name)))
  (:typescript () (text "~a" (upper-camel name)))
  (:java () (text "~a" (upper-camel name))))


(defprim bb-dynamic (name)
  (:pretty () (list 'bb-dynamic (list :name name)))
  (:typescript () (text "~a" (lower-camel name)))
  (:java () (text "~a" (lower-camel name))))

(defprim bb-element (array index)
  (:pretty () (list 'bb-element (list :array array :index (synth :pretty index))))
  (:typescript () (hcat (text "~a" (lower-camel array))
                        (brackets (synth :typescript index))))
  (:java () (hcat (text "~a" (lower-camel array))
                      (brackets (synth :java index)))))

(defprim bb-chain (&rest args)
  (:pretty () (list 'bb-chain (list :calls (synth-all :pretty (apply #'append* (rest-plain args)))
                                    :as (getf (rest-key args) :as))))
  (:typescript () (let* ((calls (synth-all :typescript (apply #'append* (rest-plain args))))
                         (as (getf (rest-key args) :as))
                         (chain (hcat (car calls) (apply #'prepend (dot) t (cdr calls)))))
                    (if as 
                        (parens (hcat (text "<~a>" (upper-camel as)) 
                                      chain))
                        chain)))
  (:java () (let* ((calls (synth-all :java (apply #'append* (rest-plain args))))
                         (as (getf (rest-key args) :as))
                         (chain (hcat (car calls) (apply #'prepend (dot) t (cdr calls)))))
                    (if as 
                        (parens (hcat+ (parens (synth :java as)) 
                                      chain))
                        chain))))

(defprim bb-constructor (name parameters &rest statements)
  (:pretty () (list 'bb-constructor (list :name name
                                          :parameters (synth-all :pretty parameters) 
                                          :statements (synth-all :pretty statements))))
  (:typescript () (vcat (hcat (text "constructor") 
                              (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t)))
  (:java () (vcat (hcat (text "public ~a" (upper-camel name)) 
                              (parens (apply #'punctuate (comma) nil (synth-all :java parameters)))) 
                        (braces 
                         (nest 4 (apply #'vcat (synth-all :java statements)))
                         :newline t))))

;; (defprim bb-arrow (parameters &rest statements)
;;   (:pretty () (list 'bb-arrow (list :parameters (synth-all :pretty parameters) 
;;                                     :statements (synth-all :pretty statements))))
;;   (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
;;                         (text " => ") 
;;                         (braces 
;;                          (nest 4 (apply #'postpend (semi) t 
;;                                         (synth-all :typescript statements)))
;;                          :newline t))))
(defprim bb-arrow (parameters expression)
  (:pretty () (list 'bb-arrow (list :parameters (synth-all :pretty parameters) 
                                    :expression (synth :pretty expression))))
  (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
                        (text " => ") 
                        (synth :typescript expression)))
  (:java () (hcat (parens (apply #'punctuate (comma) nil (synth-all :java parameters)))
                  (text " -> ") 
                  (braces (nest 4 (synth :java expression))
                          :newline t))))


(defprim bb-unit (name &rest elements)
  (:pretty () (list 'bb-unit (list :name name :elements (synth-all :pretty (apply #'append* elements)))))
  (:typescript () (apply #'vcat (synth-all :typescript (apply #'append* elements))))
  (:java () (apply #'vcat (synth-all :java (apply #'append* elements)))))

(defprim bb-return  (expression)
  (:pretty () (list 'bb-return (list :expression expression)))
  (:typescript () (hcat (text "return ")
                        (synth :typescript expression)))
  (:java () (hcat (text "return ")
                        (synth :java expression)
                        (semi))))

(defprim bb-if (expression success failure)
  (:pretty () (list 'bb-if (list :expression expression :success success :failure failure)))
  (:typescript () (error "not implemented yet"))
  (:java () (vcat (hcat (text "if") 
                        (parens (synth :java expression)))
                  (braces 
                   (nest 4 (synth :java success))
                   :newline t)
                  (text "else")
                  (braces 
                   (nest 4 (synth :java failure))
                   :newline t))))

(defmacro defop (name)
  `(defprim ,(symb "BB-" name) (op1 op2)
     (:pretty () (list ',(symb "BB-" name) (list :op1 op1 :op2 op2)))
     (:typescript () (error "not implemented yet"))
     (:java () (hcat (synth :java op1)
                     (text " ~a " ',name)
                     (synth :java op2)))))


(defop +)

(defprim bb-equal (op1 op2)
     (:pretty () (list 'bb-equal (list :op1 op1 :op2 op2)))
     (:typescript () (error "not implemented yet"))
     (:java () (hcat (synth :java op1)
                     (text " == ")
                     (synth :java op2))))
(defun bb-null (item)
  (bb-equal item (bb-nil)))

(defprim bb-nil ()
     (:pretty () (list 'bb-null))
     (:typescript () (error "not implemented yet"))
     (:java () (text "null")))

(defprim bb-throw (exception)
     (:pretty () (list 'bb-throw (list :exception exception)))
     (:typescript () (error "not implemented yet"))
     (:java () (hcat (text "throw ")
                     (synth :java exception)
                     (semi))))





