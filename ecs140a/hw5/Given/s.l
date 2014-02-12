;;; ECS 140A HW 5
;;; Hardy Jones
;;; 999397426
;;; Most of these functions are horribly inefficient
;;; but they are simpler than a tail recursive version.
;;; This is probably for the best,
;;; as the idea is to show comprehension of the material
;;; rather than understanding of algorithmic complexity.

;; Part 1

(defun double-recursive (y)
  (defun double (l)
      (cond ((null l) ())
	    (t        (list* (car l)
			     (car l)
			     (double (cdr l))))))
  ;; We need to do an initial check before we recurse.
  (if (listp y)
      (double y)
      nil))

(defun double-iterative (y)
  (defun double ()
    (do ((iter-y y (cdr iter-y))
	 (l () (list* (car iter-y) (car iter-y) l)))
	((null iter-y) (reverse l))))

  ;; We need to do an initial check before we iterate.
  (if (listp y)
      (double)
      nil))

(defun double-mapcar (y)
  (if (listp y)
      (apply #'append (mapcar (lambda (x) (list x x)) y))
      nil))

;; Part 2
(defun rdouble (y)
  (defun double (l)
    (cond ((null l)        ())
	  ((listp (car l)) (cons (rdouble (car l))
				 (rdouble (cdr l))))
	  (t               (list* (car l)
				  (car l)
				  (rdouble (cdr l))))))
  ;; We need to do an initial check before we iterate.
  (if (listp y)
      (double y)
      nil))

;; Part 3
(defun my-mapcar (fun l1 l2)
  ;; If either of the lists are empty, just return nil.
  ;; This way we keep the semantics of mapcar,
  ;; mapping to the length of the shortest list.
  (cond ((null l1) nil)
	((null l2) nil)
	(t         (cons (funcall fun (car l1) (car l2))
			 (my-mapcar fun (cdr l1) (cdr l2))))))

;; Part 4
(defun rewrite (x)
  (defun rewrite-if-then (x)
    (list (rewrite (cadr x)) 
	  (rewrite (caddr x))))
  (defun rewrite-else (x)
    (list 't (rewrite (cadddr x))))
  (cond 
    ;; It's not a list, so we can't recurse it. Just return it.
    ((not (listp x))  x)
    ;; The empty list is time to stop.
    ((null x) ())
    ;; We've got an if expression.
    ((eq 'if (car x)) 
     ;; If it's got an else part,
     (if (cadddr x)
	 ;; tack on the catchall to the cond.
	 (list 'cond (rewrite-if-then x) (rewrite-else x))
	 (list 'cond (rewrite-if-then x))))
    ;; There's some list to recurse.
    ((listp (car x))  (cons (rewrite (car x))
			    (rewrite (cdr x))))
    ;; Leave the first part alone, and rewrite the rest.
    (t (cons (car x) (rewrite (cdr x))))))

;; Part 5
(defun check (x)
  ;; Use a local binding so we only evaluate it once.
  (let* ((evaled (eval x))
	 (rewritten (eval (rewrite x))))
    ;; This should be (<are the two equal?> <eval the s-expr> <rewrite then eval the s-expr>)
    (list (equal evaled rewritten) evaled rewritten)))
