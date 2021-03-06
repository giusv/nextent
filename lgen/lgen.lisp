(in-package :lgen)
(defprim node% (name)
  (:pretty () (list 'node (list :name name))))

(setq *gensym-counter* 0)
(defmacro node (name)
  `(node% (gensym ;; (mkstr ,name)
                  )))

(defprim edge (from to &optional label)
  (:pretty () (list 'edge (list :from (synth :pretty from) :to (synth :pretty to) :label label))))

(defprim graph (nodes edges)
  (:pretty () (list 'graph (list :nodes (synth-all :pretty nodes) :edges (synth-all :pretty edges)))))

(defprim fsm (nodes edges start accept)
  (:pretty () (list 'fsm (list :nodes (synth-all :pretty nodes) :edges (synth-all :pretty edges)
                               :start (synth :pretty start)
                               :accept (synth-all :pretty accept)))))

;; (defun named-list (lst)
;;   (if (null lst)
;;       nil
;;       (append (list (keyw (synth :name))))))
;; (defmacro defsm (&rest ))
(defprim epsilon ()
  (:pretty () (list 'epsilon))
  (:nfa () (let* ((i (node 'start-empty))
                  (f (node 'accept-empty))
                  (e (edge i f))
                  ;; (g (graph (list i f)
                  ;;           (list e)))
                  )
             (fsm (list i f) (list e) i (list f)))))

(defprim sym (value)
  (:pretty () (list 'sym (list :value value)))
  (:nfa () (let* ((i (node (symb 'start "-" value)))
                  (f (node (symb 'accept "-" value)))
                  (e (edge i f value))
                  ;; (g (graph (list i f)
                  ;;           (list e)))
                  )
             (fsm (list i f) (list e) i (list f)))))

(defprim bar (&rest res)
  (:pretty () (list 'bar (list :res (synth-all :pretty res))))
  (:nfa () (let* ((nfas (synth-all :nfa res))
                  (i (node (symb 'start-bar)))
                  (f (node (symb 'accept-bar)))
                  (internal-nodes (apply #'append (synth-all :nodes nfas)))
                  (internal-edges (apply #'append (synth-all :edges nfas)))
                  (start-edges (mapcar (lambda (node) (edge i node)) 
                                       (synth-all :start nfas)))
                  (accept-edges (mapcar (lambda (node) (edge node f)) 
                                       (apply #'append (synth-all :accept nfas)))))
             (fsm (append* i f internal-nodes)
                  (append internal-edges start-edges accept-edges)
                  i (list f)))))

(defprim cat (&rest res)
  (:pretty () (list 'cat (list :res (synth-all :pretty res))))
  (:nfa () (let* ((nfas (synth-all :nfa res))
                  (nodes (apply #'append (synth-all :nodes nfas)))
                  (edges (apply #'append (synth-all :edges nfas)))
                  (connecting-edges (mapcar (lambda (pair)
                                              (edge (car (synth :accept (car pair)))
                                                    (synth :start (cadr pair))))
                                            (overlaps nfas))))
             (progn ;; (my-debug "cat"
                    ;;           nodes 
                    ;;           (append edges connecting-edges))
                    (fsm nodes (append edges connecting-edges)
                         (synth :start (car nfas)) (synth :accept (car (last nfas))))))))

(defprim star (re)
  (:pretty () (list 'star (list :re (synth :pretty re))))
  (:nfa () (let* ((nfa (synth :nfa re))
                  (i (node (symb 'start-star)))
                  (f (node (symb 'accept-star)))
                  (nodes (synth :nodes nfa))
                  (edges (append* (edge i (synth :start nfa))
                                  (edge (car (synth :accept nfa)) f)
                                  (edge (car (synth :accept nfa)) (synth :start nfa))
                                  (edge i f)
                                  (synth :edges nfa))))
             (fsm nodes edges i (list f)))))

;; (defun epsilon-closure-stack states nfa stack)
(defun epsilon-closure (states nfa)
  (labels ((epsilon-closure-single (s nfa)
             (let ((epsilon-neighbors (synth-all :to (remove-if-not
                                                      (lambda (edge)
                                                        (and (closure-equal (synth :from edge) s)
                                                             (null (synth :label edge))))
                                                      (synth :edges nfa)))))
               (progn
                 ;; (my-debug "epsilon-neighbors" s epsilon-neighbors)
                 
                 (if epsilon-neighbors 
                     (union (epsilon-closure epsilon-neighbors nfa) epsilon-neighbors)
                     ;; (apply #'append*
                     ;;        epsilon-neighbors
                     ;;        (epsilon-closure epsilon-neighbors nfa))
                     nil)))))
    (reduce #'union (mapcar (lambda (s) 
                              (epsilon-closure-single s nfa))
                            states)
            :initial-value states)))

(defun move (states label nfa)
  (labels ((move-single (s label nfa)
             (synth-all :to (remove-if-not
                             (lambda (edge)
                               (and (closure-equal (synth :from edge) s)
                                    (equal label (synth :label edge))))
                             (synth :edges nfa)))))
    (reduce #'union (mapcar (lambda (s) 
                              (move-single s label nfa))
                            states)
            :initial-value nil)))

(defun alphabet (fsm)
  (remove nil (reduce #'union 
                      (mapcar (lambda (edge)
                                (list (synth :label edge)))
                              (synth :edges fsm))
                      :initial-value nil)))

;; (defun subset-construction (nfa marked dstates dtran state)
;;   (let* ((new-marked (cons state marked))
;;          (neighbors (mapcar (lambda (sym)
;;                               (epsilon-closure (move state sym nfa) nfa)
;;                               (alphabet nfa))))
;;          (new-dstates (reduce (lambda (u))
;;                               neighbors
;;                               :initial-value dstates)))))
;; (defun nfa-to-dfa (nfa marked)
;;   (let ((start (epsilon-closure (list (synth :start nfa)))))
    
;;     ))

(defun subset (a b &key (test #'equal))
  (reduce (lambda (acc elem)
            (and acc (if (member elem b :test test) t nil)))
          a
          :initial-value t))

;; (pprint (subset '(b a) '(a b c)))


(defun set-equal (a b &key (test #'equal))
  (and (subset a b :test test)
       (subset b a :test test)))
;; (pprint (set-equal '(a b) '(b a c)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; (defun subset-construction (nfa)
;;   (let* ( ;; (test (lambda (set1 set2) (set-equal set1 set2 :test #'closure-equal)))
;;          (dstates (epsilon-closure (list (synth :start nfa)) nfa)) 
;;          (dtran nil)
;;          (unmarked dstates)
;;          (alphabet (alphabet nfa)))
;;     (while unmarked
;;       (let ((tset (pop unmarked)))
;;         (dolist (a alphabet)
;;           (let ((test (lambda (set1 set2)
;;                         (set-equal set1 set2 :test #'closure-equal)))
;;                 (uset (epsilon-closure (move tset a nfa) nfa)))
;;             (if (not (member uset dstates :test test))
;;                 (progn (setf dstates (adjoin uset dstates :test test))
;;                        (setf unmarked (adjoin uset unmarked :test test))))
;;             (setf dtran (adjoin (list tset a uset) dtran))))))
;;     (let* ((test (lambda (set1 set2)
;;                    (set-equal set1 set2 :test #'closure-equal)))
;;            (new-dstates (reduce (lambda (acc state) (acons (gensym) state acc))
;;                                 dstates
;;                                 :initial-value nil))
;;            (new-dtran (reduce (lambda (acc tran)
;;                                 (list (rassoc (car tran) new-dstates :test test)
;;                                       (cadr tran)
;;                                       (rassoc (caddr tran) new-dstates :test test)))
;;                               dtran)))
;;       (values new-dstates
;;               new-dtran))))

(defun subset-construction (nfa)
  (let* ((test (lambda (set1 set2) (set-equal set1 set2 :test #'closure-equal)))
         (dstates (list (epsilon-closure (list (synth :start nfa)) nfa))) 
         (dtran nil)
         (unmarked dstates)
         (alphabet (alphabet nfa)))
    (while unmarked
      (let ((tset (pop unmarked)))
        (dolist (a alphabet)
          (let ((test (lambda (set1 set2)
                        (set-equal set1 set2 :test #'closure-equal)))
                (uset (epsilon-closure (move tset a nfa) nfa)))
            (if (not (member uset dstates :test test))
                (progn (setf dstates (adjoin uset dstates :test test))
                       (setf unmarked (adjoin uset unmarked :test test))))
            (setf dtran (adjoin (list tset a uset) dtran))))))
    (let* ((test (lambda (set1 set2)
                   (set-equal set1 set2 :test #'closure-equal)))
           (new-dstates (reduce (lambda (acc state) (acons (gensym) state acc))
                                dstates
                                :initial-value nil))
           (new-dtran (reduce (lambda (acc tran)
                                (cons (list (car (rassoc (car tran) new-dstates :test test))
                                            (cadr tran)
                                            (car (rassoc (caddr tran) new-dstates :test test)))
                                      acc))
                              dtran
                              :initial-value nil)))
      (values new-dstates
              new-dtran))
    ;; (values dstates
    ;;         dtran)
    ))



(defun closure-equal (x y)
   (equal (synth :pretty x) (synth :pretty y)))

(let* ((re (cat (star (bar (sym 'a) (sym 'b)))
                (sym 'a) (sym 'b) (sym 'b)))
       (nfa (synth :nfa re))) 
  ;; (pprint (synth :pretty nfa))
  ;; (pprint (synth-all :pretty (epsilon-closure (list (synth :start nfa)) nfa)))
  ;; (pprint (alphabet nfa))
  ;; (print (synth-all :pretty (move (epsilon-closure (list (synth :start nfa)) nfa) 'b nfa)))
  (multiple-value-bind (dstates dtran) (subset-construction nfa)
    (labels ((pprint-state (state) 
               (format t "~a = {~{~a~^,~}}~%" (car state) (synth-all :name (cdr state)))
               ;(pprint (synth-all :name state))
               )
             (pprint-transition (transition) 
               (format t "~a -> ~a -> ~a" (car transition) (cadr transition) (caddr transition))
               ;; (pprint-state (car transition))
               ;; (pprint (cadr transition))
               ;; (pprint-state (caddr transition))
               (format t "~%-------------------~%")
               ))
      ;; (pprint (synth :pretty nfa))
      (mapcar #'pprint-state dstates)      
      (format t "~%-------------------~%")
      (mapcar #'pprint-transition dtran))))
