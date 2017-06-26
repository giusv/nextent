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

(defprim c-empty ()
  (:pretty () (list 'c-empty))
  (:c () (empty)))

(defprim c-comment (text)
  (:pretty () (list 'c-comment (list :text text))) 
  (:c () (text "//~a" (synth :string text))))

(defprim c-pair (name type &key init const)
  (:pretty () (list 'c-pair (list :name name :type (synth :pretty type) :const const)))
  (:c () (hcat (if const (text "const ") (empty))
               (synth :c type name) 
               (if init 
                   (hcat (text " = ") (synth :c init))
                   (empty)))))




(defprim c-const (lit)
  (:pretty () (list 'c-const (list :lit lit)))
  (:c () (cond ((stringp lit) (double-quotes (text "~a" lit)))
                  ((numberp lit) (text "~a" lit))
                  ((symbolp lit) (text "~a" (lower lit)))
                  (t (empty)))))

(defprim c-type (name)
  (:pretty () (list 'c-type (list :name name)))
  (:c (variable) (hcat (text "~a ~a" (lower name) (lower variable)))))

(defprim c-array-type (type n)
  (:pretty () (list 'c-array-type (list :type type)))
  (:c (variable) (hcat (synth :c type variable) (brackets (text "~a" n)))))

(defprim c-array (&rest elems)
  (:pretty () (list 'c-array (list :elems (synth-all :pretty elems))))
  (:c () (braces 
             (nest 4 (apply #'punctuate (comma) t (synth-all :c (apply #'append* elems))))
             :newline t)))


(defprim c-statement (expression)
  (:pretty () (list 'c-statement (list :expression expression)))
  (:c () (hcat (synth :c expression) (semi))))

(defprim c-list (&rest statements)
  (:pretty () (list 'c-list (list :statements (synth-all :pretty (apply #'append* statements)))))
  (:c () (apply #'vcat (synth-all :c (apply #'append* statements)))))


(defprim c-signature (name parameters rtype)
  (:pretty () (list 'c-signature (list :name name 
                                       :parameters (synth-all :pretty parameters) 
                                       :rtype rtype)))
  (:c ()  (hcat (synth :c rtype)
                (blank)
                name
                (parens (apply #'punctuate (comma) t (synth-all :c parameters))) 
                (semi))))

(defprim c-include (name)
  (:pretty () (list 'c-include (list :name name)))
  (:c () (hcat (text "#include ")
               (double-quotes(text "~a" name)))))


(defmacro defassign (name op)
  `(defprim ,(symb "C-" name) (lhs rhs &key as)
     (:pretty () (list ',(symb "C-" name) (list :lhs lhs :rhs rhs :as as)))
     (:c () (hcat (synth :c lhs)
               (text " ~a" ,op)
               (if as 
                   (hcat+ (parens (synth :c as)) 
                          (synth :c rhs))
                   (synth :c rhs))))))
(defassign assign '=)
(defassign increment '+=)
(defassign decrement '-=)

(defprim c-assign (lhs rhs &key as)
  (:pretty () (list 'c-assign (list :lhs lhs :rhs rhs)))
  (:c () (hcat (synth :c lhs)
               (text " = ")
               (if as 
                   (hcat+ (parens (synth :c as)) 
                          (synth :c rhs))
                   (synth :c rhs)))))


(defprim c-call (name &rest args)
  (:pretty () (list 'c-call (list :name name 
                                  :parameters (synth-all :pretty (rest-plain args))
                                  :as (getf (rest-key args) :as))))
  (:c () (hcat (aif (getf (rest-key args) :as)
                    (parens (synth :c it)) 
                    (empty))
               (text "~a" (lower name))
               (parens (apply #'punctuate (comma) nil (synth-all :c (rest-plain args)))))))

(defprim c-for (index start end body)
  (:pretty () (list 'c-for (list :index index
                                 :start (synth :pretty start)
                                 :end (synth :pretty end)
                                 :body (synth :pretty body))))
  (:c () (vcat (hcat+ (text "for") (parens (hcat (text "~a=" (lower index)) (synth :c start) (semi)
                                      (text "~a<" (lower index)) (synth :c end) (semi)
                                      (text "~a++" (lower index)))))
               (braces
                (nest 4 (synth :c body))
                :newline t))))

(defprim c-dynamic (name)
  (:pretty () (list 'c-dynamic (list :name name)))
  (:c () (text "~a" (lower name))))

(defprim c-element (array index)
  (:pretty () (list 'c-element (list :array array :index (synth :pretty index))))
  (:c () (hcat (text "~a" (lower array))
               (brackets (synth :c index)))))



(defprim c-unit (name &rest elements)
  (:pretty () (list 'c-unit (list :name name :elements (synth-all :pretty (apply #'append* elements)))))
  (:c () (apply #'vcat (synth-all :c (apply #'append* elements)))))

(defprim c-return  (expression)
  (:pretty () (list 'c-return (list :expression expression)))
  (:c () (hcat (text "return ")
               (synth :c expression)
               (semi))))

(defprim c-if (expression success failure)
  (:pretty () (list 'c-if (list :expression expression :success success :failure failure)))
  (:c () (vcat (hcat (text "if") 
                     (parens (synth :c expression)))
               (braces 
                (nest 4 (synth :c success))
                :newline t)
               (text "else")
               (braces 
                (nest 4 (synth :c failure))
                :newline t))))

;; (defmacro defop (name)
;;   `(defprim ,(symb "C-" name) (op1 op2)
;;      (:pretty () (list ',(symb "C-" name) (list :op1 op1 :op2 op2)))
;;      (:c () (hcat (synth :c op1)
;;                   (text " ~a " ',name)
;;                   (synth :c op2)))))


;; (defop +)

(defprim c-equal (op1 op2)
  (:pretty () (list 'c-equal (list :op1 op1 :op2 op2)))
  (:c () (hcat (synth :c op1)
               (text " == ")
               (synth :c op2))))

(defun c-null (item)
  (c-equal item (c-nil)))

(defprim c-nil ()
     (:pretty () (list 'c-null))
     (:c () (text "NULL")))
