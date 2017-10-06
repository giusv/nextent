(in-package :utils)
(defun rest-key (list)
  (cond ((null list) nil)
        ((atom list) (error "not a list"))
        ((keywordp (car list)) (cons (car list) (cons (cadr list) (rest-key (cddr list)))))
        (t (rest-key (cdr list)))))

(defun rest-plain (list)
  (cond ((null list) nil)
        ((atom list) (error "not a list"))
        ((keywordp (car list)) (rest-plain (cddr list)))
        (t (cons (car list) (rest-plain (cdr list))))))

(defun random-number (start end)
     (+ start (random (+ 1 (- end start)))))
(defun random-string (length)
  (let ((consonants "bcdfghjklmnpqrstvwxyz")
	(vowels "aeiou"))
    (concatenate 'string 
		 (loop for i from 0 to length
		    collecting (if (evenp i) 
				   (elt consonants (random 21))
				   (elt vowels (random 5)))))))
(defun random-boolean ()
  (elt (list t nil) (random 2)))

(defun ppair-p (pair)
  (and (consp pair)
       (eql (length pair) 2)
       (typep (car pair) 'keyword)))
(defun plist-p (lst)
  (every #'ppair-p (group lst 2)))


(defun plist-keys (plst)
  (loop for (key value . rest) on plst by #'cddr collect key))

(defun plist-values (plst)
  (loop for (key value . rest) on plst by #'cddr collect value))

(defun singular (sym)
  (labels ((ends-with (s end)
             (equal (subseq s (- (length s) (length end))) end)))
    (let ((s (symbol-name sym)))
      ;; (pprint (subseq s (- (length s) 2)))
      ;; (pprint (equal (subseq s (- (length s) 2)) "ES"))
      (cond ((ends-with s "IES") (concatenate 'string (subseq s 0 (- (length s) 3)) "y")) 
            ((ends-with s "SES") (subseq s 0 (- (length s) 2)))
            ((ends-with s "XES") (subseq s 0 (- (length s) 2)))
            ((ends-with s "ZES") (subseq s 0 (- (length s) 2)))
            ((ends-with s "CHES") (subseq s 0 (- (length s) 2)))
            ((ends-with s "SHES") (subseq s 0 (- (length s) 2)))
            ((ends-with s "S") (subseq s 0 (- (length s) 1)))
            (t (error "not a plural"))))))

(defmacro bindall (bindings form)
  (if (null bindings)
      form
      (let ((binding (car bindings)))
	`(multiple-value-bind ,(butlast binding) ,@(last binding)
	   (bindall ,(cdr bindings) ,form)))))

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

(defun glue (item &optional (separator ""))
  (let ((words (interleave (split-str (mkstr item)) separator)))
    (format nil "~{~a~}" words)))


(defun lower (item &optional (separator "_"))
  (let ((words (interleave (split-str (mkstr item)) separator)))

    (format nil "~(~a~)~{~a~}" (car words) (cdr words))))
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
(defmacro my-debug (message &rest vars)
  `(progn (pprint ,message)
          ,@(mapcar (lambda (var)
                      `(progn (pprint ',var)
                              (pprint (if (consp ,var)
                                          (grammar:synth-all :pretty ,var)
                                          (grammar:synth :pretty ,var)))
                              (format t "~%")))
                    vars)
          ;; (read)
          ))

(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))
(defun hash-table-values (table)
  (loop for value being the hash-values of table collect value))

(defun overlaps (lst)
  (if (or (null lst) (= 1 (length lst)))
      nil
      (cons (list (car lst) (cadr lst))
            (overlaps (cdr lst)))))

(defmacro with-multiple-value-bindings (bindings form)
  (if (null bindings)
      form
      (aif (caar bindings)
           `(multiple-value-bind ,it ,(cadar bindings)
              (with-multiple-value-bindings ,(cdr bindings) ,form))
           `(with-multiple-value-bindings ,(cdr bindings) ,form))))

;; (with-multiple-value-bindings ((nil (values 1 2))
;;                                ((c d) (values 3 4)))
;;   (pprint (list c d)))
