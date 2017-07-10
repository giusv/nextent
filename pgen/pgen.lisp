(in-package :pgen)

(defmacro my-debug (message &rest vars)
  `(progn (pprint ,message)
          ,@(mapcar (lambda (var)
                      `(progn (pprint ',var)
                              (pprint (if (consp ,var)
                                          (synth-all :pretty ,var)
                                          (synth :pretty ,var)))
                              (format t "~%")))
                    vars)))

(defun put-prod (table key value)
  (if (nth-value 1 (gethash key table))
      (setf (gethash key table) (cons value (gethash key table)))
      (setf (gethash key table) (list value))))

(defun get-prods-by-head (table key)
  (gethash key table))

(defun get-prods-by-body (table key)
  (remove-if-not (lambda (prod) 
                   (member key (synth :body prod) :test #'closure-equal))
                 (productions table)))

(defparameter *grammar* (make-hash-table))
(defmacro defproduction (head &optional rule)
  `(let ((prod (production ,head ,rule))) 
     (put-prod *grammar* (synth :symbol ,head) prod)))

(defun closure-equal (x y)
   (equal (synth :pretty x) (synth :pretty y)))

(defun is-terminal (x)
  (synth :terminal x))

(defun is-start (x)
  (synth :start x))

(defprim epsilon ()
  (:pretty () (list 'epsilon))
  (:terminal () t)
  (:call () (bb-empty)))

(defprim dollar ()
  (:pretty () (list 'dollar))
  (:terminal () t))



(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))
(defun hash-table-values (table)
  (loop for value being the hash-values of table collect value))

(defprim inher-attr (type) 
  (:pretty () (list 'inher-attr (list type (synth :pretty type)))))

(defprim synth-attr (type &rest inputs) 
  (:pretty () (list 'synth-attr (list type (synth :pretty type) :inputs (synth-plist :pretty inputs))))
  (:parameters () (synth-plist-merge (lambda (pair)
                                       (bb-pair (car pair) (synth :type (cadr pair))))
                                     inputs)))

(defprim terminal (symbol &optional type)
  (:pretty () (list 'terminal (list :symbol symbol :type type)))
  (:terminal () t))

(defprim nonterminal (symbol synthesized &key start)
  (:pretty () (list 'nonterminal (list :symbol symbol :synthesized (synth :pretty synthesized) :start start)))
  (:terminal () nil)
  (:code () 
         (let* ((prodmap (gethash symbol *ptable*))
                (cases (hash-table-keys prodmap))
                (attribute synthesized)) 
           (bb-method (textify (lower-camel symbol)) (synth :parameters attribute) 
                      (aif (synth :type attribute)
                           it
                           (bb-primitive-type 'void))
                      :throws (list (bb-static 'i-o-exception))
                      (bb-list
                       (apply #'bb-switch (bb-chain (bb-dynamic 'look) (bb-dynamic 'tag))
                              :default (bb-throw (bb-new (bb-object-type 'error) 
                                                         (reduce #'bb-+ 
                                                                 (list (bb-const (reduce #'mkstr
                                                                                         (mapcar (lambda (case)
                                                                                                   (mkstr case ", "))
                                                                                                 cases)
                                                                                         :initial-value (mkstr "Error in production for " symbol ": expecting ")))
                                                                       (bb-const "found ")
                                                                       (bb-dynamic 'look)))))
                              (mapcar (lambda (case)
                                        (bb-case (bb-chain (bb-static 'tag) (bb-enum case)) 
                                                 (progn 
                                                   ;; (my-debug "in nonterminal" (car (gethash case prodmap)))
                                                   (synth :code (car (gethash case prodmap)) attribute))))
                                      cases)))))) 
  (:type (attribute) (synth :type synthesized)))

(defparameter *nonterminals* (make-hash-table))
(defmacro defnonterminal (symbol synthesized &key start)
  `(let ((nonterm (nonterminal ',symbol ,synthesized :start ,start))) 
     (progn 
       (defparameter ,symbol nonterm)
       (setf (gethash ',symbol *nonterminals*) nonterm))))

(defprim production (head &optional rule)
  (:pretty () (list 'production (list :head (synth :pretty head) :body (synth-all :pretty (synth :body this)) :rule (synth :pretty rule))))
  (:code (attribute) (if rule 
                         (synth :code rule attribute)
                         (bb-return)))
  (:body () (aif (synth :body rule)
                 it
                 (list (epsilon))))) 

(defun first-set (x)
  (cond ((null x) nil)
        ((consp x)
         (let ((f (first-set (car x))))
           ;; (my-debug "in (consp x)" (car x) f (member (epsilon) f :test #'closure-equal) (set-difference f (list (epsilon))))
           (if (member (epsilon) f :test #'closure-equal)
               (union (set-difference f (list (epsilon))) (first-set (cdr x)) :test #'closure-equal)
               f)))
        (t (if (is-terminal x)
               (list x)
               (let ((productions (get-prods-by-head *grammar* (synth :symbol x))))
                 ;; (pprint (synth-all :pretty productions))
                 (reduce (lambda (acc prod)
                           ;; (pprint (synth :pretty prod)) 
                           (aif (synth :body prod)
                                (progn ;; (my-debug "in aif" prod it (first-set it))
                                       
                                       (union acc (first-set it) :test #'closure-equal))
                                (adjoin acc (epsilon) :test #'closure-equal)))
                         productions
                         :initial-value nil))))))
(defun follow-set (x)
  (let ((productions (remove-if 
                      (lambda (prod) (closure-equal x (synth :head prod))) 
                      (get-prods-by-body *grammar* x))))
    ;; (my-debug "starting follow-set" x productions)
    (reduce (lambda (acc prod)
              (let* ((head (synth :head prod))
                     (body (synth :body prod))
                     (beta (cdr (member x body :test #'closure-equal)))
                     (follow2 (set-difference (first-set beta) (list (epsilon)) :test #'closure-equal))
                     (follow3 (if (or (null beta)
                                      (member (epsilon) (first-set beta) :test #'closure-equal))
                                  (union follow2 (follow-set head))
                                  follow2)))
                ;; (my-debug "in let* follow-set" head body beta (first-set beta) follow2 follow3)
                (union acc follow3 :test #'closure-equal)))
            productions
            :initial-value (if (is-start x)
                               (list (dollar))
                               nil))))

(defun productions (grammar)
  (flatten (hash-table-values grammar)))

(defun nonterminals (grammar)
  (reduce (lambda (acc prod)
            (adjoin (synth :head prod) acc :test #'closure-equal)) 
          (productions grammar)
          :initial-value nil))


(defun make-ptable (grammar)
  (let ((ptable (make-hash-table))
        (nonterms (nonterminals grammar)))
    (mapcar (lambda (nonterm)
              (setf (gethash (synth :symbol nonterm) ptable) (make-hash-table)))
            nonterms)
    (mapcar (lambda (prod)
              (let* ((head (synth :head prod))
                     (body (synth :body prod))
                     (f (first-set body)))
                (mapcar (lambda (term)
                          (if (closure-equal term (epsilon))
                              (let ((g (follow-set head)))
                                (mapcar (lambda (term)
                                          (if (closure-equal term (dollar)) 
                                              (put-prod (gethash (synth :symbol head) ptable) 'eof prod)
                                              (put-prod (gethash (synth :symbol head) ptable) (synth :symbol term) prod)))
                                        g))
                              (put-prod (gethash (synth :symbol head) ptable) (synth :symbol term) prod)))
                        f)))
            (productions grammar))
    ptable))

(defun pprint-ptable (ptable)
  (let ((nonterms (loop for key being the hash-keys of ptable collect key)))
    (format t "~%")
    (mapcar (lambda (nonterm) 
              (format t "nonterminal ~a:~%" nonterm)
              (let ((terms (loop for key being the hash-keys of (gethash nonterm ptable) collect key)))
                (mapcar (lambda (term)
                          (format t "~tterminal ~a:~a~%" 
                                  term
                                  (let ((prods (gethash term (gethash nonterm ptable)))) 
                                    (if (> (length prods) 1)
                                        (error "not LL(1)") 
                                        (synth :pretty (car prods)))))
                          (format t "~%"))
                        
                        terms))) 
            nonterms)))


;; (defprim with-inherited% (bindings expr)
;;   (:pretty () (list 'with-inherited (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
;;   (:code (attribute) (bb-list (synth-all :code bindings attribute) 
;;                                 (if expr (synth :code expr attribute)
;;                                     (bb-return))))
;;   (:body () (synth :body expr)))

(defprim binding% (lhs rhs)
  (:pretty () (list 'binding (list :lhs lhs :rhs (synth :pretty rhs))))
  (:code (attribute) (synth :code rhs attribute lhs))
  (:body () (synth :body rhs)))

(defprim with-bindings% (bindings expr)
  (:pretty () (list 'with-bindings (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:code (attribute) (bb-list (synth-all :code bindings attribute)
                              (if expr 
                                  (synth :code expr attribute)
                                  (bb-break))))
  (:body () (remove nil (synth-all :body bindings))))

(defprim synthesize (expr)
  (:pretty () (list 'synthesize (list :expr (synth :pretty expr))))
  (:code (attribute) (bb-return expr))
  (:body () nil))

(defmacro with-bindings ((&rest bindings) expr)
  `(let* ,(mapcar #`(,(car a1) (bb-dynamic ',(car a1))) (remove-if (lambda (bind) (= 1 (length bind)))
                                                                   bindings))
     (with-bindings% (list ,@(mapcar (lambda (a1) 
                                 `(binding%
                                   ,@(if (atom (car a1))
                                         `(',(car a1) ,(cadr a1))
                                         `(nil ,(car a1)))))
                               bindings))
       ,expr)))


(defmacro with-inherited ((&rest bindings)  expr)
  `(let* ,(mapcar #`(,(car a1) (bb-dynamic ',(car a1))) bindings)
     ;; (with-inherited% (list ,@(mapcar #`(binding% ',(car a1) (getf (synth :inputs ,head) ,(cadr a1))) bindings))
     ;;   ,expr)
     ,expr
     ))


(defprim invoke (symbol &rest parameters)
  (:pretty () (list 'invoke (list :symbol (synth :pretty symbol) :parameters (synth-all :pretty parameters))))
  (:code (attribute lhs) 
         (if lhs
             (bb-statement (bb-pair lhs (synth :type this attribute) 
                                    :init (if (is-terminal symbol)
                                              (error "terminal symbol in invoke")
                                              (apply #'bb-call (synth :symbol symbol) parameters))))
             (bb-statement (apply #'bb-call (synth :symbol symbol) parameters))))
  (:type (attribute) (synth :type symbol attribute))
  (:body () symbol))

(defprim match (symbol)
  (:pretty () (list 'match (list :symbol (synth :pretty symbol))))
  (:code (attribute lhs) 
         (if (is-terminal symbol)
             (if (and (synth :type symbol) lhs)
                 (bb-list (bb-statement (bb-pair lhs (synth :type symbol) 
                                                 :init (bb-new (bb-object-type 'identifier)
                                                               (bb-dynamic 'look :as (bb-object-type 'word))))) 
                          (bb-statement (bb-call 'match (bb-chain (bb-static 'tag) (bb-enum (synth :symbol symbol))))))
                 (bb-statement (bb-call 'match (bb-chain (bb-static 'tag) (bb-enum (synth :symbol symbol))))))
             (error "nonterminal symbol in match")))
  (:body () symbol))
(defprim lexval (symbol type)
  (:pretty () (list 'lexval (list :symbol (synth :pretty symbol) :type (synth :pretty type))))
  (:code (attribute lhs) (if (is-terminal symbol)
                             (bb-list 
                              (bb-statement (bb-pair lhs type 
                                                     :init (bb-new type (bb-chain (bb-dynamic 'look :as (bb-object-type 'num)) 
                                                                                  (bb-dynamic 'value)))))
                              (synth :code (match symbol) attribute nil))
                             (error "nonterminal symbol in lexval")))
  (:body () symbol))

(defprim lookup (symbol)
  (:pretty () (list 'lookup (list :symbol (synth :pretty symbol))))
  (:code (attribute lhs) (let ((type (synth :type symbol)))
                           (if (and (is-terminal symbol)
                                    (not (null type)))
                               (bb-list 
                                (bb-statement (bb-pair lhs type :init (bb-chain (bb-dynamic 'top) 
                                                                                (bb-call 'get (bb-chain (bb-dynamic 'look :as (bb-object-type 'word)) (bb-dynamic 'lexeme))) 
                                                                                :as type)))
                                (bb-if (bb-null (bb-dynamic lhs))
                                       (bb-statement (bb-call 'error (bb-+ (bb-chain (bb-dynamic 'look)
                                                                                     (bb-call 'to-string))
                                                                           (bb-const " undeclared"))))) 
                                (synth :code (match symbol) attribute nil))
                               (error "nonterminal symbol in lookup"))))
  (:body () symbol))
(defprim push-environment ()
  (:pretty () (list 'push-environment))
  (:code (attribute lhs) (bb-list (bb-statement (bb-pair 'saved (bb-object-type 'env) :init (bb-dynamic 'top)))
                                  (bb-statement (bb-assign (bb-dynamic 'top) (bb-new (bb-object-type 'env) (bb-dynamic 'top))))))
  (:body () nil))

(defprim pop-environment ()
  (:pretty () (list 'pop-environment))
  (:code (attribute lhs) (bb-statement (bb-assign (bb-dynamic 'top) (bb-dynamic 'saved))))
  (:body () nil))

(defprim store (id value)
  (:pretty () (list 'store (list :id (synth :pretty id) :value (synth :pretty value))))
  (:code (attribute) (bb-list
                      (bb-statement (bb-chain (bb-dynamic 'top) 
                                              (bb-call 'put (bb-chain id (bb-call 'get-id) (bb-dynamic 'lexeme)) value)))
                      (bb-break)))
  (:body () nil))

;; (defparameter etype (bb-template-type 'expression (bb-object-type 'float)))
(defparameter t-arith (bb-object-type 'arithmetic-expression))
(defparameter t-bool (bb-primitive-type 'boolean))
(defparameter t-string (bb-object-type 'string))
(defparameter t-id (bb-object-type 'identifier))
(defparameter terminal-id (terminal :id t-id))
;; (defparameter expr (bb-object-type 'expression))

(defnonterminal ee (synth-attr t-arith) :start t)
(defnonterminal ep (synth-attr t-arith 
                               :expr (inher-attr t-arith)))
(defnonterminal tt (synth-attr t-arith))
(defnonterminal tp (synth-attr t-arith
                               :expr (inher-attr t-arith)))
(defnonterminal ff (synth-attr t-arith))

(defnonterminal indy-let (synth-attr t-arith))
(defnonterminal indy-binds nil)
(defnonterminal indy-binds-rest nil)
(defnonterminal indy-bind nil)



(defnonterminal argomento (synth-attr t-string))
(defnonterminal indicatore (synth-attr t-bool) :start t)



(defproduction argomento 
    (with-bindings ((sogg (match (terminal :soggetto t-string))))
      (synthesize sogg)))

(defproduction argomento 
    (with-bindings ((veic (match (terminal :veicolo t-string))))
      (synthesize veic)))

(defproduction indicatore 
    (with-bindings ((id (match terminal-id))
                    (arg (invoke argomento)))
      (synthesize arg)))


(defproduction indy-let ;; ((terminal :let) indy-binds (terminal :in) ee)
  (with-bindings (((match (terminal :let)))
                  ((push-environment))
                  ((invoke indy-binds))
                  ((match (terminal :in)))
                  (node (invoke ee))
                  ((pop-environment)))
    (synthesize node)))

(defproduction indy-bind ;; ((terminal :id t-id) (terminal :assign) ee)
  (with-bindings ((id (match terminal-id))
                  ((match (terminal :assign)))
                  (node (invoke ee)))
    (store id node)))

(defproduction indy-binds-rest ;; ((epsilon))
  )

(defproduction indy-binds-rest ;; ((terminal :comma) indy-bind indy-binds-rest)
  (with-bindings (((match (terminal :comma)))
                  ((invoke indy-bind))
                  ((invoke indy-binds-rest)))
    nil))

(defproduction indy-binds ;; (indy-bind indy-binds-rest)
  (with-bindings (((invoke indy-bind))
                  ((invoke indy-binds-rest)))
    nil))

;; (defproduction ee (indy-let)  
;;   (with-bindings ((node (invoke indy-let)))
;;     (synthesize node)))

(defproduction ee ;; (tt ep)
  (with-bindings ((node (invoke tt))
                  (syn (invoke ep node)))
    (synthesize syn)))

(defproduction ep ;; ((terminal :plus) tt ep)
  (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :plus)))
                      (node (invoke tt))
                      (syn (invoke ep (bb-new t-arith (bb-+ (bb-chain expr (bb-call 'get-value)) 
                                                                           (bb-chain node (bb-call 'get-value)))))))
        (synthesize syn))))

(defproduction ep ;; ((epsilon))
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction tt ;; (ff tp)
  (with-bindings ((node (invoke ff))
                  (syn (invoke tp node)))
    (synthesize syn)))

(defproduction tp ;; ((terminal :times) ff tp)
  (with-inherited ((expr :expr))
    (with-bindings (((match (terminal :times)))
                    (node (invoke ff))
                    (syn (invoke tp (bb-new t-arith (bb-* (bb-chain expr (bb-call 'get-value)) 
                                                                         (bb-chain node (bb-call 'get-value)))))))
      (synthesize syn))))
 
(defproduction tp ;; ((epsilon))
  (with-inherited ((expr :expr))
    (synthesize expr)))

(defproduction ff ;; ((terminal :left) ee (terminal :right))
   (with-bindings (((match (terminal :left)))
                   (node (invoke ee))
                   ((match (terminal :right))))
    (synthesize node)))

(defproduction ff ;; ((terminal :num))
  (with-bindings ((node (lexval (terminal :num) t-arith)))
    (synthesize node)))

(defproduction ff ;; ((terminal :id t-arith))
  (with-bindings ((node (lookup (terminal :id t-arith))))
    (synthesize node)))

;; (defproduction ff (indy-let)
;;   (with-bindings ((node (invoke indy-let)))
;;     (synthesize node)))




(defparameter *ptable* (make-ptable *grammar*))

;; (pprint (synth-all :pretty (first-set ee)))
;; (pprint (synth-all :pretty (follow-set ff)))
;; (pprint (synth-all :pretty (nonterminals *grammar*)))
;; (pprint-ptable *ptable*)

;; (pprint (synth-all :pretty (apply #'append (synth-all :body (get-prods-by-head *grammar* (synth :symbol ff))))))
(write-file "D:/giusv/temp/temp.java"
            (synth :string (apply #'vcat (synth-all :doc (synth-all :java (synth-all :code (hash-table-values *nonterminals*)))))))

;; (pprint (synth-all :output (synth-all :java (synth-all :code (list ee ep tt tp ff))) 0))
;; (pprint (synth-all :pretty (get-prods-by-head *grammar* (synth :symbol ee))))
