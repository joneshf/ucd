; test everything
(defun test ()
        (format t "~%~10TBEGIN TESTING~2%")
        (test-double)
        (test-rdouble)
        (test-my-mapcar)
        (test-rewrite)
        (test-check)
        (format t "~%~10TDONE TESTING~2%")
        t
)

; some shorthands, so that you can just type, e.g., (td)
(defun td () (test-double))
(defun tr () (test-rdouble))
(defun tm () (test-my-mapcar))
(defun tw () (test-rewrite))
(defun tc () (test-check))

; use function ty -- i.e., (ty) -- to test everything and create Output.your
; then exit from clisp.
; then do a "diff Output.correct Output.your"
; (or a "tkdiff Output.correct Output.your")
; files should be identical.
;
; Output.correct was generated after
; (setq *print-case* :downcase)
; so, you might want to invoke clisp via "clisp -q -i init.lsp"
; where init.lsp is in Given.
;
; NB:  clisp output formatting takes into account the number of columns
; in your window.  The Output.correct file was generated
; for 80 column output.  Your output may differ for different sized windows.
; It's okay if your output is the same except for whitespace formatting
; (but easier, so you can use diff, if you run in an 80 column window).
;
; delete-file is needed because some clisps (e.g., the one on Linux PC)
; append rather than truncate when dribbling to an existing file.
(defun ty ()
        (delete-file "Output.your") (dribble "Output.your") (test) (dribble))


; test all the double functions
(defun test-double ()
        (format t "~%~20TTEST-DOUBLE~2%")
        (test-double-recursive)
        (test-double-mapcar)
        (test-double-iterative)
        t
)
(defun test-double-recursive ()
        (test-a-double 'double-recursive)
        t
)
(defun test-double-mapcar ()
        (test-a-double 'double-mapcar)
        t
)
(defun test-double-iterative ()
        (test-a-double 'double-iterative)
        t
)

(defun test-rdouble ()
        (format t "~%~20TTEST-RDOUBLE~2%")
        (test-a-double 'rdouble)
        t
)


; tests the double function double-funx.
; prints out double-funx instead of actual name,
; e.g., (double-funx nil nil) instead of (double-recursive nil nil)
;
(defun test-a-double (double-funx)
        (format t "~%~30TTEST-A-DOUBLE on:  ~A~2%" double-funx)
        (defun double-fun (y) (funcall double-funx y))
        (tester '(double-fun nil))
        (tester '(double-fun 44))
        (tester '(double-fun 'a))
        (tester '(double-fun '(44)))
        (tester '(double-fun '(44 55)))
        (tester '(double-fun '(1 2 3)))
        (tester '(double-fun '(11 12 13 14)))
        (tester '(double-fun '(1 (2 3) 4)))
        (tester '(double-fun '(1 (2) (3 4) (5 6 7) (8 (9 10) 11))))
        (tester '(double-fun '((111 112) (120) () (140))))
        (tester '(double-fun '(((111 112)) ((120)) (()) (140))))
        (tester '(double-fun '(1 (2 3) ((((4)))) ) ))
        t
)

(defun test-my-mapcar ()
        (format t "~%~20TTEST-MY-MAPCAR~2%")
        (tester '(my-mapcar #'+ '(1 2) '(3 4) ))
        (tester '(my-mapcar #'+ '() '() ))
        (tester '(my-mapcar #'+ '(1 2 3) '(4 5) ))
        (tester '(my-mapcar #'* '(1 2 3) '(4 5 6 7) ))
        (tester '(my-mapcar #'* '() '(4 5 6 7) ))
        (tester '(my-mapcar #'* '(1 2 3) '() ))
        (tester '(my-mapcar #'cons '( (1) (2 3)) '( (a b) (c) ) ))
        (tester '(my-mapcar #'list '( (1) (2 3)) '( (a b) (c) ) ))
        (tester '(my-mapcar #'(lambda (a b) (+ (* 2 a a) (* b b)))
                            '(1 2 3) '(4 5 6)))
        t
)

(defun test-rewrite ()
        (format t "~%~20TTEST-REWRITE~2%")
        (tester '(rewrite nil))
        (tester '(rewrite 44))
        (tester '(rewrite '(* 44 2)))
        (tester '(rewrite 'a))
        (tester '(rewrite '(list 3 4) ))
        (tester '(rewrite '(if 3 4) ))
        (tester '(rewrite '(if nil 4) ))
        (tester '(rewrite '(if 3 4 5) ))
        (tester '(rewrite '(if (if 3 8 9) 4 5) ))
        (tester '(rewrite '(if nil 4 5) ))
        (tester '(rewrite '(* 44 (if 3 2 5))))
        (tester '(rewrite '(* 44 (if nil 7 10))))
        (tester '(rewrite '(if (= 8 (if 3 8 9)) 4 5) ))
        (tester '(rewrite '(if (= 8 (if nil 8 9)) 4 5) ))
        (tester '(rewrite '(if (= 8 (if 3 (if (= 7 7) 8 1) 9)) 4 5) ))
        (tester '(rewrite '(if (= 8 (if 3 (if (= 7 6) 8
                                                      (if (+ 1 22) 3 77)
                                          )
                                          9
                                    )
                               )
                               5
                           )
                 )
        )
        (tester '(rewrite '(cons (if (= 8 7) 'bad 'good)
                                 (if (= 8 7) 'nope 'yup)
                           )
                 )
        )
        (tester '(rewrite '(list (if (= 8 7) 'bad)
                                 (if (= 8 7) 'nope)
                         )
                 )
        )
        (tester '(rewrite '(list (if (= 8 8) 'yes)
                                 (if (= 8 8) 'okay)
                           )
                 )
        )
        (tester '(rewrite '(list (if (= 8 8) 'yes)
                                 (if (= 8 7) 'nope)
                           )
                 )
        )
        t
)

(defun test-check ()
        (format t "~%~20TTEST-CHECK~2%")
        (tester '(check   nil))
        (tester '(check   44))
        (tester '(check   '(* 44 2)))
        (tester '(check   '(list 3 4) ))
        (tester '(check   '(if 3 4) ))
        (tester '(check   '(if nil 4) ))
        (tester '(check   '(if 3 4 5) ))
        (tester '(check   '(if (if 3 8 9) 4 5) ))
        (tester '(check   '(if nil 4 5) ))
        (tester '(check   '(* 44 (if 3 2 5))))
        (tester '(check   '(* 44 (if nil 7 10))))
        (tester '(check   '(if (= 8 (if 3 8 9)) 4 5) ))
        (tester '(check   '(if (= 8 (if nil 8 9)) 4 5) ))
        (tester '(check   '(if (= 8 (if 3 (if (= 7 7) 8 1) 9)) 4 5) ))
        (tester '(check   '(if (= 8 (if 3 (if (= 7 6) 8
                                                      (if (+ 1 22) 3 77)
                                          )
                                          9
                                    )
                               )
                               5
                           )
                 )
        )
        (tester '(check   '(cons (if (= 8 7) 'bad 'good)
                                 (if (= 8 7) 'nope 'yup)
                           )
                 )
        )
        (tester '(check   '(list (if (= 8 7) 'bad)
                                 (if (= 8 7) 'nope)
                           )
                 )
        )
        (tester '(check   '(list (if (= 8 8) 'yes)
                                 (if (= 8 8) 'okay)
                           )
                 )
        )
        (tester '(check   '(list (if (= 8 8) 'yes)
                                 (if (= 8 7) 'nope)
                           )
                 )
        )
        t
)


; low-level testing routine
(defun tester (x)
        (format t "testing:  ~A~%" x)
        ; split into 2 so print value even if eval fails.
        (format t "value:    ")
        (format t "~A~2%" (eval x))
        t
)
