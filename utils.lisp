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
  (let ((s (symbol-name sym)))
    (if (eql (car (last s)) \#S)
        (symb (butlast s))
        (error "not plural"))))
