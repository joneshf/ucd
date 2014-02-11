;;; ECS 140A HW 5
;;; Hardy Jones
;;; 999397426

;; Part 1
(defun double-recursive (y)
  (cond ((null y) ())
	(t        (cons (car y) 
			(cons (car y) 
			      (double-recursive (cdr y)))))))
