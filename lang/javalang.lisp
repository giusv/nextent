(in-package :java)


(defprim bb-empty ()
  (:pretty () (list 'bb-empty)) 
  (:java () (empty)))

(defprim bb-comment (text)
  (:pretty () (list 'bb-comment (list :text text))) 
  (:java () (text "//~a" (synth :string text))))

(defprim bb-pair (name type &key init const private)
  (:pretty () (list 'bb-pair (list :name name :type (synth :pretty type) :init (synth :pretty init) :const const))) 
  (:java () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (synth :java type) 
                        (text " ~a" (lower-camel name))
                        (if init 
                            (hcat (text " = ") (synth :java init))
                            (empty)))))




(defprim bb-const (lit)
  (:pretty () (list 'bb-const (list :lit lit))) 
  (:java () (cond ((stringp lit) (double-quotes (text "~a" lit)))
                  ((numberp lit) (text "~a" lit))
                  ((symbolp lit) (text "~a" (lower-camel lit)))
                  (t (empty)))))




(defprim bb-try (body catches &optional finally)
  (:pretty () (list 'bb-try (list :body (synth :pretty body) :catches (synth-plist :pretty catches)))) 
  (:java () (apply #'vcat (text "try")
                   (braces 
                    (nest 4 (synth :java body))
                    :newline t)
                   (synth-all :java catches))))

(defprim bb-catch% (exceptions name body)
  (:pretty () (list 'bb-catch (list :exceptions exceptions :name name :body (synth :pretty body)))) 
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
  (:java () (hcat (text "~a" (lower-camel name)))))

(defprim bb-array-type (type)
  (:pretty () (list 'bb-array-type (list :type type))) 
  (:java () (hcat (synth :java type) 
                  (text "[]"))))

(defprim bb-object-type (name)
  (:pretty () (list 'bb-object-type (list :name name))) 
  (:java () (textify (upper-camel name))))

(defprim bb-wildcard-type ()
  (:pretty () (list 'bb-wildcard-type)) 
  (:java () (text "?")))

(defprim bb-template-type (name &rest types)
  (:pretty () (list 'bb-template-type (list :name name :types (synth-all :pretty types)))) 
  (:java () (hcat (textify (upper-camel name))
                  (angular (apply #'punctuate (comma) nil (mapcar (lambda (type)
                                                            (synth :java type))
                                                          types))))))

(defprim bb-type (name &key primitive array template)
  (:pretty () (list 'bb-type (list :name name :primitive primitive :array array :template template))) 
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
  (:java () (braces 
             (nest 4 (apply #'punctuate (comma) t (synth-all :java (apply #'append* elems))))
             :newline t)))

(defprim bb-object (&rest elems)
  (:pretty () (list 'bb-object (list :elems (synth-plist :pretty (apply #'append* elems))))) 
  (:java () (apply #'punctuate (comma) t 
                   (synth-plist-merge 
                    #'(lambda (pair) (hcat (textify (car pair)) (equals) (synth :java (cadr pair)))) 
                    (apply #'append* elems)))))

(defprim bb-template (element)
  (:pretty () (list 'bb-template (list :element (synth :pretty element)))) 
  (:java () (error "not available in java")))

(defprim bb-with-annotations (annotations expr &key (newline t))
  (:pretty () (list 'bb-with-annotation (list :annotations (synth-all :pretty annotations) :expr (synth :pretty expr)))) 
  (:java () (apply (if newline #'vcat #'hcat+) (append* (synth-all :java annotations)
                                                           (synth :java expr)))))

(defprim bb-annotation (name &rest props)
  (:pretty () (list 'bb-annotation (list name name :props (synth-plist :pretty props)))) 
  (:java () (hcat (text "@~a" name) 
                  (cond ((null props) (empty))
                        ((plist-p props) (parens
                                          (apply #'punctuate (comma) nil 
                                                 (synth-plist-merge 
                                                  #'(lambda (pair) (hcat (text "~a = " (first pair))
                                                                         (second pair))) 
                                                  props))))
                        ((and (listp props) (= (length props) 1)) (parens (car props)))
                        ((listp props) (parens (braces (apply #'punctuate (comma) nil props))))
                        (t (error "case not allowed"))))))

(defprim bb-annotation2 (name &optional content)
  (:pretty () (list 'bb-annotation2 (list name name :content (synth :pretty content)))) 
  (:java () (hcat (text "@~a" name) 
                  (cond ((null content) (empty))
                        (t (parens (synth :java content)))))))

(defprim bb-class (name &key public interfaces parent fields constructor methods)
  (:pretty () (list 'bb-class (list :name name 
                                    :public public
                                    :interfaces interfaces 
                                    :parent parent 
                                    :fields (synth-all :pretty fields) 
                                    :constructor (synth :pretty constructor)
                                    :methods (synth-all :pretty methods)))) 
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
  (:java () (hcat (synth :java expression) (semi))))

(defprim bb-list (&rest statements)
  (:pretty () (list 'bb-list (list :statements (synth-all :pretty (apply #'append* statements))))) 
  (:java () (apply #'vcat (synth-all :java (apply #'append* statements)))))

(defprim bb-method (name parameters rtype &rest args)
  (:pretty () (list 'bb-method (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype
                                     :throws (synth-all :pretty (getf (rest-key args) :throws))
                                     :statements (synth-all :pretty (rest-plain args))))) 
  (:java ()  (vcat (hcat (text "public ") 
                        (synth :java rtype)
                        (blank)
                        name
                        (parens (apply #'punctuate (comma) t (synth-all :java parameters)))
                        (aif (getf (rest-key args) :throws) 
                             (hcat+ (text " throws") 
                                    (apply #'punctuate (comma) t (synth-all :java it))))) 
                  (braces 
                   (nest 4 (apply #'vcat (synth-all :java (rest-plain args))))
                   :newline t))))

(defprim bb-signature (name parameters rtype)
  (:pretty () (list 'bb-signature (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype))) 
  (:java ()  (hcat (text "public ") 
                  (synth :java rtype)
                  (blank)
                  name
                  (parens (apply #'punctuate (comma) t (synth-all :java parameters))) 
                  (semi))))

(defprim bb-import (name &rest elements)
  (:pretty () (list 'bb-import (list :name (synth :pretty name) 
                                     :elements elements))) 
  (:java () (mapcar (lambda (elem)
                      (hcat (text "import ~a.~a" name elem)
                            (semi)))
                    elements)))

(defprim bb-package (name)
  (:pretty () (list 'bb-package (list :name name))) 
  (:java () (text "package ~a;" name)))

(defprim bb-assign (lhs rhs &key as)
  (:pretty () (list 'bb-assign (list :lhs lhs :rhs rhs))) 
  (:java () (hcat (synth :java lhs)
                        (text " = ")
                        (if as (parens (text "~a" (upper-camel as))))
                        (synth :java rhs))))

(defprim bb-new (type &rest parameters)
  (:pretty () (list 'bb-new (list :type (synth :pretty type) 
                                  :parameters (synth-all :pretty parameters)))) 
  (:java () (hcat (text "new ")
                        (synth :java type)
                        (parens (apply #'punctuate (comma) nil (synth-all :java parameters))))))

(defprim bb-call (name &rest args)
  (:pretty () (list 'bb-call (list :name name 
                                   :parameters (synth-all :pretty (rest-plain args))
                                   :as (getf (rest-key args) :as)))) 
  (:java () (hcat (aif (getf (rest-key args) :as)
                       (hcat (parens (text "~a" (upper-camel as))) (blank))
                       (empty))
                  (text "~a" (lower-camel name))
                  (parens (apply #'punctuate (comma) nil (synth-all :java (rest-plain args)))))))
(defprim bb-static (name)
  (:pretty () (list 'bb-static (list :name name))) 
  (:java () (text "~a" (upper-camel name))))


(defprim bb-dynamic (name &key as)
  (:pretty () (list 'bb-dynamic (list :name name :as as))) 
  (:java () (if as
                (parens (hcat+ (parens (synth :java as)) (text "~a" (lower-camel name))))
                (text "~a" (lower-camel name)))))

(defprim bb-enum (name)
  (:pretty () (list 'bb-enum (list :name name))) 
  (:java () (text "~a" (string-upcase name))))

(defprim bb-element (array index)
  (:pretty () (list 'bb-element (list :array array :index (synth :pretty index)))) 
  (:java () (hcat (text "~a" (lower-camel array))
                      (brackets (synth :java index)))))

(defprim bb-chain (&rest args)
  (:pretty () (list 'bb-chain (list :calls (synth-all :pretty (apply #'append* (rest-plain args)))
                                    :as (getf (rest-key args) :as)))) 
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
  (:java () (hcat (parens (apply #'punctuate (comma) nil (synth-all :java parameters)))
                  (text " -> ") 
                  (braces (nest 4 (synth :java expression))
                          :newline t))))


(defprim bb-unit (name &rest elements)
  (:pretty () (list 'bb-unit (list :name name :elements (synth-all :pretty (apply #'append* elements))))) 
  (:java () (apply #'vcat (synth-all :java (apply #'append* elements)))))

(defprim bb-return (&optional expression)
  (:pretty () (list 'bb-return (list :expression expression))) 
  (:java () (if expression (hcat (text "return ")
                                 (synth :java expression)
                                 (semi))
                (hcat (text "return") (semi)))))

(defprim bb-if (expression success &optional failure)
  (:pretty () (list 'bb-if (list :expression expression :success success :failure failure))) 
  (:java () (vcat (hcat (text "if") 
                        (parens (synth :java expression)))
                  (braces 
                   (nest 4 (synth :java success))
                   :newline t)
                  (if failure 
                      (vcat (text "else")
                            (braces 
                             (nest 4 (synth :java failure))
                             :newline t))))))

(defprim bb-switch (expression &rest cases)
  (:pretty () (list 'bb-switch (list :expression expression 
                                     :cases (synth-all :pretty (rest-plain cases)) 
                                     :default (getf (rest-key cases) :default)))) 
  (:java () (vcat (hcat (text "switch") 
                        (parens (synth :java expression)))
                  (braces 
                   (nest 4 (apply #'vcat (append (synth-all :java (rest-plain cases))
                                                 (list (vcat (hcat (text "default") (colon))
                                                             (braces 
                                                              (nest 4 (vcat (synth :java (getf (rest-key cases) :default))))
                                                              :newline t))))))
                   :newline t))))

(defprim bb-break ()
  (:pretty () (list 'bb-break)) 
  (:java () (text "break;")))

(defprim bb-case (expression statement)
  (:pretty () (list 'bb-case (list :expression expression :statement statement))) 
  (:java () (vcat (hcat (text "case ") (synth :java expression) (colon))
                  (braces 
                   (nest 4 (synth :java statement))
                   :newline t)
                  ;; (synth :java (bb-break))
                  ))) 

(defmacro defop (name)
  `(defprim ,(symb "BB-" name) (op1 op2)
     (:pretty () (list ',(symb "BB-" name) (list :op1 op1 :op2 op2))) 
     (:java () (hcat (synth :java op1)                     (text " ~a " ',name)
                     (synth :java op2)))))


(defop +)
(defop -)
(defop *)
(defop /)

;; (defprim bb-equal (op1 op2)
;;      (:pretty () (list 'bb-equal (list :op1 op1 :op2 op2))) ;;      
;;      (:java () (hcat (synth :java op1)
;;                      (text " == ")
;;                      (synth :java op2))))
(defun bb-null (item)
  (bb-equal item (bb-nil)))

(defprim bb-nil ()
  (:pretty () (list 'bb-null)) 
     (:java () (text "null")))

(defprim bb-throw (exception)
  (:pretty () (list 'bb-throw (list :exception exception))) 
     (:java () (hcat (text "throw ")
                     (synth :java exception)
                     (semi))))





(defmacro defbexp (operator &optional representation (arity 0))
  (let ((name (symb "BB-" operator)))
    `(defprim ,name 
         ,(if (eq arity 'unbounded)
              `(&rest exps)
              (loop for i from 1 to arity collect (symb "EXP" i)))
       (:pretty () (list ',name 
			 ,(if (eq arity 'unbounded)
			      `(list :exps (synth-all :pretty exps))
			      `(list ,@(apply #'append (loop for i from 1 to arity collect (list (keyw "EXP" i) `(synth :pretty ,(symb "EXP" i))))))))) 
       (:java () ,(cond ((eq arity 'unbounded) `(apply #'doc:punctuate (doc:text " ~a " ,representation) nil (synth-all :java exps)))
                        ((eq arity 0) `(doc:text "~a" ,representation))
                        ((eq arity 1) `(doc:hcat (doc:text "~a " ,representation) (synth :java ,(symb "EXP1"))))
                        (t `(doc:punctuate (doc:text " ~a " ,representation) nil ,@(loop for i from 1 to arity collect `(synth :java ,(symb "EXP" i))))))))))


(defmacro defbexps (&rest bexps)
  `(progn
     ,@(mapcar #'(lambda (bexp)
		   `(defbexp ,(car bexp) ,@(cdr bexp)))
	       bexps)))

;;(def-bexp true)
;; (def-bexp equal 2)

(defbexps (true '|true|) (false '|false|) (and '&& 2) (or '\|\| 2) (not '! 1) (equal '== 2) (less-than '< 2) (greater-than '> 2))
