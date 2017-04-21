(in-package :doc)

(defprim empty ()
  (:pretty () `(empty))
  (:output (*) ())
  (:string () ())
  (:doc () this)
  (:extent () 0))

(defprim text (template &rest args)
  (:pretty () `(text (:template ,template :args ,args)))
  (:output (indent) (format t "~v,0t~?" indent template args))
  (:string () (with-output-to-string (*standard-output*)
                (synth :output this 0)))
  ;; (:doc () (apply #'text template args))
  (:doc () this)
  (:extent () (length (apply #'format nil template args))))

(defprim nest (amount doc)
  (:pretty () `(nest (:amount ,amount :doc ,(synth :pretty doc))))
  (:output (indent) (synth :output doc (+ indent amount)))
  (:string () (with-output-to-string (*standard-output*)
			(synth :output this 0)))
  (:doc () this)
  (:extent () (+ amount (synth :extent doc))))

(defprim vcat (&rest docs)
  (:pretty () `(vcat (:docs ,(synth-all :pretty docs))))
  (:output (indent) (let ((fdocs (flatten docs)))
		     (unless (null fdocs) 
		       (progn (synth :output (car fdocs) indent)
			      (unless (null (cdr fdocs)) 
				(progn (format t "~%"))
				(synth :output (apply #'vcat (cdr fdocs)) indent))))))
  (:string () (with-output-to-string (*standard-output*)
			(synth :output this 0)))
  (:doc () this)  
  (:extent () (let ((fdocs (flatten docs)))
		     (synth :extent (car (last fdocs))))))

(defprim hcat (&rest docs)
  (:pretty () `(hcat (:docs ,(synth-all :pretty docs))))
  (:output (indent) (let ((fdocs (flatten docs)))
		     (unless (null fdocs) 
		     	 (progn (synth :output (car fdocs) indent)
		     		(synth :output (apply #'hcat (cdr fdocs)) (+ indent (synth :extent (car fdocs))))))))
  (:string () (with-output-to-string (*standard-output*)
			(synth :output this 0)))
  (:doc () (apply #'hcat docs))
  (:extent () (let ((fdocs (flatten docs)))
	       (reduce #'+ (synth-all :extent fdocs)))))

(defun append* (&rest args)
  (let ((args* (mapcar (lambda (arg) 
                         (cond ((null arg) nil)
                               ((atom arg) (list arg))
                               (t arg)))
                       args)))
    (apply #'append args*)))

(defmacro vcat-all (fn lst)
  `(apply #'vcat (mapcar #',fn ,lst)))

(defun wrap (doc start end &key newline (padding 0))
(if newline 
      (vcat start doc end)
      (hcat start (padding padding) doc (padding padding) end)))

(defmacro defwrapper (name start end)
  `(defun ,name (doc &key newline (padding 0))
     (wrap doc (text ,start) (text ,end) :newline newline :padding padding)))

(defwrapper parens "(" ")")
(defwrapper brackets "[" "]")
(defwrapper braces "{" "}")
(defwrapper single-quotes "'" "'")
(defwrapper double-quotes "\"" "\"")
(defwrapper back-quotes "`" "`")

(defun padding (p)
  (text "~a" (make-string p :initial-element #\Space)))

(defun comma ()
  (text ","))
(defun dot ()
  (text "."))
(defun semi ()
  (text ";"))
(defun colon ()
  (text ":"))
(defun forward-slash ()
  (text "/"))
(defun equals () 
  (text "="))

(defun punctuate (p newline &rest docs)
  (cond ((null docs) nil)
	((eq 1 (length docs)) (car docs))
	(t (if newline
	       (vcat (hcat (car docs) p) (apply #'punctuate p newline (cdr docs)))
	       (hcat (car docs) p (apply #'punctuate p newline (cdr docs)))))))

(defun prepend (p newline &rest docs)
  (cond ((null docs) (empty)) 
	((eq 1 (length docs)) (hcat p (car docs)))
        (t (if newline
               (vcat (hcat p (car docs)) (apply #'prepend p newline (cdr docs)))
	       (hcat p (car docs) (apply #'prepend newline (cdr docs)))))))

(defun postpend (p newline &rest docs)
  (cond ((null docs) (empty)) 
	((eq 1 (length docs)) (hcat (car docs) p))
        (t (if newline
	       (vcat (hcat (car docs) p) (apply #'postpend p newline (cdr docs)))
	       (hcat (car docs) p (apply #'postpend newline (cdr docs)))))))


(defun split-str-1 (string &optional (separator "-") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun split-str (string &optional (separator "-"))
  (split-str-1 string separator))

(defun interleave (lst sep)
  (if (equal 1 (length lst))
      lst
      (cons (car lst) (cons sep (interleave (cdr lst) sep)))))

(defun lower-camel (item &optional (separator ""))
  (let ((words (interleave (mapcar #'string-capitalize (split-str (mkstr item))) separator)))
    (format nil "~(~a~)~{~a~}" (car words) (cdr words))))

(defun upper-camel (item &optional (separator ""))
  (let ((words (interleave (mapcar #'string-capitalize (split-str (mkstr item))) separator)))
    (format nil "~{~a~}" words)))

(defun write-file (name content)
    (with-open-file (stream name
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)
    (format stream content)))
