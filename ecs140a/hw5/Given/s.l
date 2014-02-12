;;; ECS 140A HW 5
;;; Hardy Jones
;;; 999397426

;; Part 1

(defun double-recursive (y)
  (cond ((null y) ())
	(t        (cons (car y)
			(cons (car y)
			      (double-recursive (cdr y)))))))

(defun double-iterative (y)
  (do ((iter-y y (cdr iter-y))
       (l () (append (list (car iter-y) (car iter-y))
		     l)))
      ((null iter-y) l)))

(defun double-mapcar (y)
  (apply 'append (mapcar (lambda (x) (list x x)) y)))
