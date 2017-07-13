(in-package :grammar)

;; (defmacro defprim (name args &rest attrs)
;;   `(defun ,name ,args
;;      (pandoriclet ,(mapcar #`(,a1 ,a1) (parser:arg-names args))
;;                   (dlambda ,@(mapcar #`(,(car a1) ,(cadr a1) ,(list `(declare (ignorable) ,@(parser:arg-names (cadr a1))) (caadr a1))) attrs)
;;                            (t (&rest args) (apply this args))))))




(defmacro defprim (name lambda-list &body attrs)
  (let ((args (parser:arg-names lambda-list)))
    `(defun ,name ,lambda-list
       (declare (optimize debug))
       (pandoriclet ,(mapcar #`(,a1 ,a1) args)
                    (dlambda ,@(mapcar #`(,(keyw a1) nil ,a1 ;; (get-pandoric ,name ,a1)
                                           ) args)
                             ,@attrs
                             (t (&rest *) ;; (apply lol:this args)
                                (error "attribute not found")))))))
(defmacro defprod (nonterm (name lambda-list &rest attrs))
  `(defprim ,name ,(untype-lambdalist lambda-list)
     ,@(cons `(:nonterm ()
                        (if (and 
                             ,@(mapcar (lambda (arg)
                                         (if (atom (cadr arg)) 
                                             `(eq (synth :nonterm ,(car arg)) ,(cadr arg))
                                             `(every (lambda (nont)
                                                       (eq (synth :nonterm nont) ,(cadadr arg)))
                                                     ,(car arg))))
                                       (args lambda-list)))
                            ,nonterm
                            (error "bbb")))
             attrs)))

;; (defprod :expression (and ((req :integer) &rest (expressions (list :expression)))))

(defun synth (att box &rest args)
  (if box (apply box att args)))

(defun synth-all (att boxlist &rest args)
  (if boxlist (mapcar (lambda (box) (apply #'synth att box args))
           boxlist)))

(defun synth-plist (func plst &rest args)
  (apply #'append (mapcar (lambda (pair) (list (car pair) (apply #'synth func (cadr pair) args)))
			   (group plst 2))))

(defun synth-plist-merge (func plst &rest args)
  (mapcar (lambda (pair) (apply func pair args))
	   (group plst 2)))


(defun untype-lambdalist (tlist)
  (mapcar (lambda (elem)
            (cond ((or (eq elem '&optional)
                       (eq elem '&rest)
                       (eq elem '&key))
                   elem)
                  ((consp elem)
                   (car elem))))
          tlist))

(defun args (tlist)
  (mapcar (lambda (elem)
            (if (atom (car elem))
                elem
                (list (caar elem) (cadr elem))))
          (remove-if (lambda (elem)
                             (or (eq elem '&optional)
                                 (eq elem '&rest)
                                 (eq elem '&key)))
                        tlist)))

;; (pprint (untype-lambdalist '((name :string) &rest ((types nil types-supplied-p) :integer))))

;; (pprint (args '((name :string) &optional ((opt 0) :string) &rest ((types nil types-supplied-p) (list :integer)) &key (key1 :string) ((key2 8 key2-supplied-p) :string))))


;; (defprim text (template &rest args)
;;   (:pretty () `(text (:template ,template :args ,args))))
;; (defprim vcat (&rest docs)
;;   (:pretty () `(vcat (:docs ,(synth-all :pretty docs))))
;;   (:output (indent) (let ((fdocs (flatten docs)))
;; 		     (unless (null fdocs) 
;; 		       (progn (synth :output (car fdocs) indent)
;; 			      (unless (null (cdr fdocs)) 
;; 				(progn (format t "~%"))
;; 				(synth :output (apply #'vcat (cdr fdocs)) indent))))))
;;   (:string (indent) (with-output-to-string (*standard-output*)
;; 			(synth :output this indent)))
;;   (:doc () this)  
;;   (:extent () (let ((fdocs (flatten docs)))
;; 		     (synth :extent (car (last fdocs))))))

;; (defun dtext (template &rest args)
;;   (pandoriclet ((template template)
;;                 (args args))
;;     (dlambda 
;;      (:pretty () `(text (:template ,template :args ,args)))
;;      (:self () this)
;;      (t (&rest args)
;;         (apply this args)))))

;; (defun ptext (template &rest args)
;;   (let ((template template)
;;         (args args))
;;     (plambda () (template args)
;;      (dlambda
;;       (:pretty () `(text (:template ,template :args ,args)))
;;       (:self () this)
;;       (t (&rest args)
;;          (apply this args))))))

;; (setf (symbol-function 'pptext) (ptext "ptext3: ~a" 30))
;; (with-pandoric (template args) #'pptext
;;   (format t "template: ~a, args: ~{~a~}~%" template args))
;; (pptext :pandoric-set 'args (list 31))
;; (funcall #'pptext :pandoric-set 'template "aa")
;; (with-pandoric (template args) #'pptext
;;   (format t "template: ~a, args: ~{~a~}~%" template args))


;; (let ((dtextmod (funcall (dtext "ptext2: ~a" 30) :pandoric-set 'template "aa")))
;;   (with-pandoric (template args) dtextmod
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (let* ((dtest (dtext "ptext2: ~a" 30))
;;        (dtestmod (funcall dtest :pandoric-set 'template "aa")))
;;   (with-pandoric (template args) dtestmod
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (let* ((ptest (ptext "ptext: ~a" 1))
;;        (dtest (dtext "dtext: ~a" 2)))
;;   (with-pandoric (template args) ptest
;;     (format t "template: ~a, args: ~{~a~}~%" template args))
;;   (with-pandoric (template args) (funcall ptest :pandoric-set 'args 31)
;;     (format t "template: ~a, args: ~{~a~}~%" template args))
;;   (with-pandoric (template args) dtest
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (setf (symbol-function 'test) 
;;       (text "~a" 1))
;; (pprint (test :pretty))

;; (let* ((test (text "~a" 1))
;;        (test2 (funcall test :self)))
;;   (pprint (funcall test :pandoric-get 'template)))

;; (pprint (funcall (text "~a" 2) :pretty))
