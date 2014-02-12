;;; ECS 140A HW 5
;;; Hardy Jones
;;; 999397426

;; Part 1

(defun double-recursive (y)
  (defun double (l)
      (cond ((null l) ())
	    (t        (cons (car l)
			    (cons (car l)
				  (double (cdr l)))))))
  (if (listp y)
      (double y)
      nil))

(defun double-iterative (y)
  (defun double ()
    (do ((iter-y y (cdr iter-y))
	 (l () (append l
		       (list (car iter-y) (car iter-y)))))
	((null iter-y) l)))

  (if (listp y)
      (double)
      nil))

(defun double-mapcar (y)
  (if (listp y)
      (apply 'append (mapcar (lambda (x) (list x x)) y))
      nil))

;; Part 2
