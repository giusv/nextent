(in-package :java)


(defprim java-empty ()
  (:pretty () (list 'java-empty)) 
  (:java () (empty)))

(defprim java-comment (text)
  (:pretty () (list 'java-comment (list :text text))) 
  (:java () (text "//~a" (synth :string text))))

(defprim java-pair (name type &key init const private)
  (:pretty () (list 'java-pair (list :name name :type (synth :pretty type) :init (synth :pretty init) :const const))) 
  (:java () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (synth :java type) 
                        (text " ~a" (lower-camel name))
                        (if init 
                            (hcat (text " = ") (synth :java init))
                            (empty)))))




(defprim java-const (lit)
  (:pretty () (list 'java-const (list :lit lit))) 
  (:java () (cond ((stringp lit) (double-quotes (text "~a" lit)))
                  ((numberp lit) (text "~a" lit))
                  ((symbolp lit) (text "~a" (lower-camel lit)))
                  (t (empty)))))




(defprim java-try (body catches &optional finally)
  (:pretty () (list 'java-try (list :body (synth :pretty body) :catches (synth-plist :pretty catches)))) 
  (:java () (apply #'vcat (text "try")
                   (braces 
                    (nest 4 (synth :java body))
                    :newline t)
                   (synth-all :java catches))))

(defprim java-catch% (exceptions name body)
  (:pretty () (list 'java-catch (list :exceptions exceptions :name name :body (synth :pretty body)))) 
  (:java () (vcat (hcat+ (text "catch")
                         (parens (hcat+ (apply #'punctuate (text " | ") nil (mapcar (lambda (e)
                                                                       (textify (upper-camel e)))
                                                                     exceptions))
                                        (textify (lower-camel name))))) 
                  (braces 
                   (nest 4 (synth :java body))
                   :newline t))))

(defmacro java-catch (exceptions body)
  `(let* ((,(car exceptions) ',(car exceptions))) 
     (java-catch% (list ,@(cdr exceptions)) ,(car exceptions) ,body)))

(defprim java-primitive-type (name)
  (:pretty () (list 'java-primitive-type (list :name name))) 
  (:java () (hcat (text "~a" (lower-camel name)))))

(defprim java-array-type (type)
  (:pretty () (list 'java-array-type (list :type type))) 
  (:java () (hcat (synth :java type) 
                  (text "[]"))))

(defprim java-object-type (name)
  (:pretty () (list 'java-object-type (list :name name))) 
  (:java () (textify (upper-camel name))))

(defprim java-wildcard-type ()
  (:pretty () (list 'java-wildcard-type)) 
  (:java () (text "?")))

(defprim java-template-type (name &rest types)
  (:pretty () (list 'java-template-type (list :name name :types (synth-all :pretty types)))) 
  (:java () (hcat (textify (upper-camel name))
                  (angular (apply #'punctuate (comma) nil (mapcar (lambda (type)
                                                            (synth :java type))
                                                          types))))))

(defprim java-type (name &key primitive array template)
  (:pretty () (list 'java-type (list :name name :primitive primitive :array array :template template))) 
  (:java () (hcat (text "~a" (if (or (eq name :int)
                                     (eq name :float)
                                     (eq name :double)
                                     (eq name :void)) 
                                 (lower-camel name)
                                 (upper-camel name)))
                  (if array (brackets (empty)) (empty))
                  (if template (angular (synth :java template))))))


;; (defprim java-bool (value)
;;   (:pretty () (list 'java-bool (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim java-number (value)
;;   (:pretty () (list 'java-number (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim java-string (value)
;;   (:pretty () (list 'java-string (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

(defprim java-array (&rest elems)
  (:pretty () (list 'java-array (list :elems (synth-all :pretty elems)))) 
  (:java () (braces 
             (nest 4 (apply #'punctuate (comma) t (synth-all :java (apply #'append* elems))))
             :newline t)))

(defprim java-object (&rest elems)
  (:pretty () (list 'java-object (list :elems (synth-plist :pretty (apply #'append* elems))))) 
  (:java () (apply #'punctuate (comma) t 
                   (synth-plist-merge 
                    #'(lambda (pair) (hcat (textify (car pair)) (equals) (synth :java (cadr pair)))) 
                    (apply #'append* elems)))))

(defprim java-template (element)
  (:pretty () (list 'java-template (list :element (synth :pretty element)))) 
  (:java () (error "not available in java")))

(defprim java-with-annotations (annotations expr &key (newline t))
  (:pretty () (list 'java-with-annotation (list :annotations (synth-all :pretty annotations) :expr (synth :pretty expr)))) 
  (:java () (apply (if newline #'vcat #'hcat+) (append* (synth-all :java annotations)
                                                           (synth :java expr)))))

(defprim java-annotation (name &rest props)
  (:pretty () (list 'java-annotation (list name name :props (synth-plist :pretty props)))) 
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

(defprim java-annotation2 (name &optional content)
  (:pretty () (list 'java-annotation2 (list name name :content (synth :pretty content)))) 
  (:java () (hcat (text "@~a" name) 
                  (cond ((null content) (empty))
                        (t (parens (synth :java content)))))))

(defprim java-class (name &key public interfaces parent fields constructor methods)
  (:pretty () (list 'java-class (list :name name 
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

(defprim java-interface (name &key public interfaces methods)
  (:pretty () (list 'java-interface (list :name name 
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



(defprim java-statement (expression)
  (:pretty () (list 'java-statement (list :expression expression))) 
  (:java () (hcat (synth :java expression) (semi))))

(defprim java-list (&rest statements)
  (:pretty () (list 'java-list (list :statements (synth-all :pretty (apply #'append* statements))))) 
  (:java () (apply #'vcat (synth-all :java (apply #'append* statements)))))

(defprim java-method (name parameters rtype &rest args)
  (:pretty () (list 'java-method (list :name name 
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

(defprim java-signature (name parameters rtype)
  (:pretty () (list 'java-signature (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype))) 
  (:java ()  (hcat (text "public ") 
                  (synth :java rtype)
                  (blank)
                  name
                  (parens (apply #'punctuate (comma) t (synth-all :java parameters))) 
                  (semi))))

(defprim java-import (name &rest elements)
  (:pretty () (list 'java-import (list :name (synth :pretty name) 
                                     :elements elements))) 
  (:java () (mapcar (lambda (elem)
                      (hcat (text "import ~a.~a" name elem)
                            (semi)))
                    elements)))

(defprim java-package (name)
  (:pretty () (list 'java-package (list :name name))) 
  (:java () (text "package ~a;" name)))

(defprim java-assign (lhs rhs &key as)
  (:pretty () (list 'java-assign (list :lhs lhs :rhs rhs))) 
  (:java () (hcat (synth :java lhs)
                        (text " = ")
                        (if as (parens (text "~a" (upper-camel as))))
                        (synth :java rhs))))

(defprim java-new (type &rest parameters)
  (:pretty () (list 'java-new (list :type (synth :pretty type) 
                                  :parameters (synth-all :pretty parameters)))) 
  (:java () (hcat (text "new ")
                        (synth :java type)
                        (parens (apply #'punctuate (comma) nil (synth-all :java parameters))))))

(defprim java-call (name &rest args)
  (:pretty () (list 'java-call (list :name name 
                                     :parameters (synth-all :pretty (rest-plain args))
                                     :as (getf (rest-key args) :as)))) 
  (:java () (hcat (aif (getf (rest-key args) :as)
                       (hcat (parens (text "~a" (upper-camel it))) (blank))
                       (empty))
                  (text "~a" (lower-camel name))
                  (parens (apply #'punctuate (comma) nil (synth-all :java (rest-plain args)))))))
(defprim java-static (name)
  (:pretty () (list 'java-static (list :name name))) 
  (:java () (text "~a" (upper-camel name))))


(defprim java-dynamic (name &key as)
  (:pretty () (list 'java-dynamic (list :name name :as as))) 
  (:java () (if as
                (parens (hcat+ (parens (synth :java as)) (text "~a" (lower-camel name))))
                (text "~a" (lower-camel name)))))

(defprim java-enum (name)
  (:pretty () (list 'java-enum (list :name name))) 
  (:java () (text "~a" (string-upcase name))))

(defprim java-element (array index)
  (:pretty () (list 'java-element (list :array array :index (synth :pretty index)))) 
  (:java () (hcat (text "~a" (lower-camel array))
                      (brackets (synth :java index)))))

(defprim java-chain (&rest args)
  (:pretty () (list 'java-chain (list :calls (synth-all :pretty (apply #'append* (rest-plain args)))
                                    :as (getf (rest-key args) :as)))) 
  (:java () (let* ((calls (synth-all :java (apply #'append* (rest-plain args))))
                         (as (getf (rest-key args) :as))
                         (chain (hcat (car calls) (apply #'prepend (dot) t (cdr calls)))))
                    (if as 
                        (parens (hcat+ (parens (synth :java as)) 
                                      chain))
                        chain))))

(defprim java-constructor (name parameters &rest statements)
  (:pretty () (list 'java-constructor (list :name name
                                          :parameters (synth-all :pretty parameters) 
                                          :statements (synth-all :pretty statements)))) 
  (:java () (vcat (hcat (text "public ~a" (upper-camel name)) 
                              (parens (apply #'punctuate (comma) nil (synth-all :java parameters)))) 
                        (braces 
                         (nest 4 (apply #'vcat (synth-all :java statements)))
                         :newline t))))

;; (defprim java-arrow (parameters &rest statements)
;;   (:pretty () (list 'java-arrow (list :parameters (synth-all :pretty parameters) 
;;                                     :statements (synth-all :pretty statements))))
;;   (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
;;                         (text " => ") 
;;                         (braces 
;;                          (nest 4 (apply #'postpend (semi) t 
;;                                         (synth-all :typescript statements)))
;;                          :newline t))))
(defprim java-arrow (parameters expression)
  (:pretty () (list 'java-arrow (list :parameters (synth-all :pretty parameters) 
                                    :expression (synth :pretty expression)))) 
  (:java () (hcat (parens (apply #'punctuate (comma) nil (synth-all :java parameters)))
                  (text " -> ") 
                  (braces (nest 4 (synth :java expression))
                          :newline t))))


(defprim java-unit (name &rest elements)
  (:pretty () (list 'java-unit (list :name name :elements (synth-all :pretty (apply #'append* elements))))) 
  (:java () (apply #'vcat (synth-all :java (apply #'append* elements)))))

(defprim java-return (&optional expression)
  (:pretty () (list 'java-return (list :expression expression))) 
  (:java () (if expression (hcat (text "return ")
                                 (synth :java expression)
                                 (semi))
                (hcat (text "return") (semi)))))

(defprim java-if (expression success &optional failure)
  (:pretty () (list 'java-if (list :expression expression :success success :failure failure))) 
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

(defprim java-switch (expression &rest cases)
  (:pretty () (list 'java-switch (list :expression expression 
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

(defprim java-break ()
  (:pretty () (list 'java-break)) 
  (:java () (text "break;")))

(defprim java-case (expression statement)
  (:pretty () (list 'java-case (list :expression expression :statement statement))) 
  (:java () (vcat (hcat (text "case ") (synth :java expression) (colon))
                  (braces 
                   (nest 4 (synth :java statement))
                   :newline t)
                  ;; (synth :java (java-break))
                  ))) 

(defmacro defop (name)
  `(defprim ,(symb "JAVA-" name) (op1 op2)
     (:pretty () (list ',(symb "JAVA-" name) (list :op1 op1 :op2 op2))) 
     (:java () (hcat (synth :java op1)                     (text " ~a " ',name)
                     (synth :java op2)))))


(defop +)
(defop -)
(defop *)
(defop /)

;; (defprim java-equal (op1 op2)
;;      (:pretty () (list 'java-equal (list :op1 op1 :op2 op2))) ;;      
;;      (:java () (hcat (synth :java op1)
;;                      (text " == ")
;;                      (synth :java op2))))
(defun java-null (item)
  (java-equal item (java-nil)))

(defprim java-nil ()
  (:pretty () (list 'java-null)) 
     (:java () (text "null")))

(defprim java-throw (exception)
  (:pretty () (list 'java-throw (list :exception exception))) 
     (:java () (hcat (text "throw ")
                     (synth :java exception)
                     (semi))))





(defmacro defbexp (operator &optional representation (arity 0))
  (let ((name (symb "JAVA-" operator)))
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
